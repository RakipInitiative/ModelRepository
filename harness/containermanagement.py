import json
import glob
import subprocess
import requests
import zipfile
import csv
import time
import logging
import os
import re
import xml.etree.ElementTree as ET
from urllib.parse import urlparse
from pathlib import Path
import libsedml
import s3_upload

output_path = "/data"

def parse_metadata_rdf(filePath):
    # Parse the metadata.rdf file to extract model and visualization script paths
    metadata_rdf_path = os.path.join(filePath, 'metadata.rdf')
    
    if not os.path.exists(metadata_rdf_path):
        logging.warning(f"metadata.rdf not found at {metadata_rdf_path}, falling back to default script names")
        return {
            'modelScript': os.path.join(filePath, 'model'),
            'visualizationScript': os.path.join(filePath, 'visualization')
        }
    
    try:
        # Register namespaces
        namespaces = {
            'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
            'dc': 'http://purl.org/dc/elements/1.1/'
        }
        
        # Parse the XML file
        tree = ET.parse(metadata_rdf_path)
        root = tree.getroot()
        
        # Initialize script paths with default values
        script_paths = {
            'modelScript': os.path.join(filePath, 'model'),
            'visualizationScript': os.path.join(filePath, 'visualization')
        }
        
        # Find all Description elements
        for desc in root.findall('.//{http://www.w3.org/1999/02/22-rdf-syntax-ns#}Description'):
            # Get the about attribute (file path)
            about = desc.get('{http://www.w3.org/1999/02/22-rdf-syntax-ns#}about')
            if about and about != '.':
                # Find the type element
                type_elem = desc.find('.//{http://purl.org/dc/elements/1.1/}type')
                if type_elem is not None and type_elem.text:
                    # If it's a model script or visualization script, store the path
                    if type_elem.text == 'modelScript':
                        script_paths['modelScript'] = os.path.join(filePath, about.lstrip('/'))
                    elif type_elem.text == 'visualizationScript':
                        script_paths['visualizationScript'] = os.path.join(filePath, about.lstrip('/'))
        
        logging.debug(f"Found script paths in metadata.rdf: {script_paths}")
        return script_paths
    
    except Exception as e:
        logging.error(f"Error parsing metadata.rdf: {e}")
        # Fall back to default script names
        return {
            'modelScript': os.path.join(filePath, 'model'),
            'visualizationScript': os.path.join(filePath, 'visualization')
        }

def run_simulation(modelLocation, config):
    # runs the simulation
    # params: parameters given via endpoint/api

    params = config.get("parameters", {})
    simulation_id = params.get("simulation_id", None)

    # download fskx file
    filePath = modelLocation  #params['modelLocation']
    # download_fskx_file(filePath, params)

    make_output_dir()

    # merge defaulSimulation.py, model.py and visualization.py into fullSimulation.py (for .r respectively)
    simulationScriptPath = prepare_simulation_script(filePath, config)
    
    if simulationScriptPath.rsplit('.')[-1] == 'py':
        # subprocess.Popen(f"""echo "backend: Agg" > ~/.config/matplotlib/matplotlibrc""", shell=True, cwd=filePath)
        process = subprocess.Popen(f"/usr/local/bin/python {simulationScriptPath} 1", shell=True, cwd=filePath, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    elif simulationScriptPath.rsplit('.')[-1] == 'r':
        process = subprocess.Popen(f"Rscript {simulationScriptPath} 1", shell=True, cwd=filePath, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    
    stdout, stderr = process.communicate()
    
    if process.returncode != 0:
        logging.error(stderr)
        write_log_file(stderr)
        return stderr
    write_log_file(stdout)
    write_epcis_event_file(json.loads(params.get("request_event", "{}")))
    try:
        s3_upload.upload_directory(output_path, config["result_bucket_s3"], simulation_id)
        s3_upload.upload_directory(filePath, config["result_bucket_s3"], simulation_id)
    except Exception as e:
        logging.error(f"Error uploading to S3: {e}")
    

def make_output_dir():
    dir = f"{output_path}"
    if not os.path.exists(dir):
        os.makedirs(dir)


def write_log_file(log):
    log_path = f"{output_path}/simulation_log.log"
    with open(log_path, "wb") as outfile:
        outfile.write(log)

def write_epcis_event_file(request_event):
    event_path = f"{output_path}/request_event.json"
    with open(event_path, "w") as outfile:
        outfile.write(json.dumps(request_event))


def download_fskx_file(filePath, params):
    # downloads fskx file from zenodo repository using record number. saves and extracts file in filePath
    output_file_name = filePath + '/sample_file.fskx'
    modelUrl = params['modelUrl'] 
    req = requests.get(modelUrl)
    time.sleep(5)
    with open(output_file_name, "wb") as outfile:
        outfile.write(req.content)

    extract_zip=True    
    if extract_zip:
        with zipfile.ZipFile(output_file_name, 'r') as zip_ref:
            zip_ref.extractall(filePath)

    return 


def prepare_simulation_script(filePath, config):
        # merges defaultSimulation script, model script and visualization script into fullSimulation script

        params = config.get("parameters", {})
        plot_type = params.get("plot_type", "png")

        logging.debug("Building full simulation script.")
        fileExtension = get_file_extension(filePath)
        logging.debug(f"Found the file extension {fileExtension}")

        # Create the simulation directory if it doesn't exist
        if not os.path.exists(filePath + '/simulations'):
            os.makedirs(filePath + '/simulations')
        extract_params = True
        if extract_params:
            logging.info("Extracting parameters")
            extract_model_parameters(filePath, fileExtension, params)
            
        # Get script paths from metadata.rdf
        script_paths = parse_metadata_rdf(filePath)
        
        parameterScriptPath = filePath + '/simulations/defaultSimulation' + fileExtension
        modelScriptPath = script_paths['modelScript']
        visualizationScriptPath = script_paths['visualizationScript']
        fullSimulationScriptPath = filePath + '/fullSimulation' + fileExtension
        
        # Check if the model script exists with the extension
        if not os.path.exists(modelScriptPath) and '.' not in os.path.basename(modelScriptPath):
            modelScriptPath = modelScriptPath + fileExtension
            
        # Check if the visualization script exists with the extension
        if not os.path.exists(visualizationScriptPath) and '.' not in os.path.basename(visualizationScriptPath):
            visualizationScriptPath = visualizationScriptPath + fileExtension
        
        logging.debug(f"Using model script: {modelScriptPath}")
        logging.debug(f"Using visualization script: {visualizationScriptPath}")
        
        filenames = [parameterScriptPath, modelScriptPath]
        if plot_type and os.path.exists(visualizationScriptPath):
            filenames.append(visualizationScriptPath)
        with open(fullSimulationScriptPath, 'w') as outfile:
            if fileExtension == '.py':
                outfile.write("import matplotlib\nmatplotlib.use('agg')\n")
            elif fileExtension == '.r':
                plot_code = save_plot_code_r(plot_type)
                outfile.write(plot_code)
            for fname in filenames:
                with open(fname) as infile:
                    outfile.write(infile.read())
                    outfile.write("\n")
            if fileExtension == '.py':
                save_vars_code = save_output_vars_python()
                outfile.write(save_vars_code)
                plot_code = save_plot_code_python(plot_type)
                outfile.write(plot_code)
            elif fileExtension == '.r':
                save_vars_code = save_output_vars_r()
                outfile.write(save_vars_code)
            
        logging.info("Full simulation script built and saved under " + fullSimulationScriptPath)
        return fullSimulationScriptPath


def get_file_extension(filePath):
    # retrieves the file extension of the model script
    script_paths = parse_metadata_rdf(filePath)
    model_script_path = script_paths['modelScript']
    
    # If the path doesn't include an extension, try to find the file
    if '.' not in os.path.basename(model_script_path):
        possible_files = glob.glob(f"{model_script_path}.*")
        if possible_files:
            model_script_path = possible_files[0]
    
    logging.debug(f"Model script path for extension detection: {model_script_path}")
    return os.path.splitext(model_script_path)[1]


def extract_default_parameters(filePath, config_parameters):
    sedml_path = filePath + '/sim.sedml'
    
    try:
        # Load the SED-ML file
        reader = libsedml.SedReader()
        sedml_doc = reader.readSedML(sedml_path)

        # Get the list of models
        #models = sedml_doc.getListOfModels()
        # get the simulation with "simulationName" from helm config
        default = sedml_doc.getModel('defaultSimulation')
        change_attributes = default.getListOfChanges()
        # Iterate over the change attributes
        input_vars_dict = {}
        for change_attribute in change_attributes:
            pName = change_attribute.getTarget()
            pValue = change_attribute.getNewValue()
            input_vars_dict[pName] = pValue
            #print(f"{pName} {equals} {pValue}")
        
        
    except FileNotFoundError:
       logging.info(f"JSON file {sedml_path} not found.")
    except Exception as e:
        logging.info("An error occurred in reading model parameters from sim.sedml:" + e)
    return input_vars_dict


def extract_model_parameters(filePath, fileExtension, api_params):
    # extracts model's parameters & values from metaData.json file and api call. Adds in simulations/defaultSimulation.py(r)
    json_file_name = filePath + '/metaData.json'
    output_file_name = filePath + '/simulations/defaultSimulation' + fileExtension
    if fileExtension=='.py':
        equals = "="
    elif fileExtension=='.r':
        equals = "<-"
    try:
        input_vars_dict = extract_default_parameters(filePath, api_params)
        with open(json_file_name, 'r') as json_file:
            data = json.load(json_file)
        params_lst=data['modelMath']['parameter']
        with open(output_file_name, 'w') as output_file:
            output_file.write("\n")
            for item in params_lst:
                if item['classification'] != 'OUTPUT':
                    var_id = item['id']
                    var_value = input_vars_dict[var_id]
                    if var_id in api_params:
                        var_value = api_params[var_id]
                        
                    # check if parameter is a URL, then download that into workspace
                    if item['dataType'] == 'FILE':
                        parsed_url = urlparse(var_value)
                        if parsed_url.scheme and parsed_url.netloc:
                            downloadUrlParameter(filePath, var_value)
                            var_value = "'" + get_filename_and_extension(var_value) + "'" # replace url with plain filename
                        #FILE parameters require quotation marks, otherwise they are evaluated as variables which throws error
                        else:
                            if "'" not in var_value and "\"" not in var_value:
                                var_value = "'" + var_value + "'"
                    input_vars_dict[var_id]=var_value
                    output_file.write(f"{var_id} {equals} {var_value}\n")
        save_input_vars(input_vars_dict)
        logging.info(f"Model's parameters extracted and written successfully to: {output_file_name}")
    except FileNotFoundError:
       logging.info(f"JSON file {json_file_name} not found.")
    except Exception as e:
        logging.info("An error occurred in reading model parameters from metaData.json:" + e)
    return


def save_input_vars(input_vars_dict):

    input_var_path = f"{output_path}/input.json"

    with open(input_var_path, "w") as outfile:
        outfile.write(json.dumps(input_vars_dict))


def get_model_parameters(filePath):
    # returns model's parameters & values from metaData.json file 
    result_dict = {}
    json_file_name = filePath + '/metaData.json'
    try:
        with open(json_file_name, 'r') as json_file:
            data = json.load(json_file)
        params_lst=data['modelMath']['parameter']
        result_dict = {item["id"]: item["value"] for item in params_lst if item["classification"] != "OUTPUT"}
    except FileNotFoundError:
       logging.info(f"JSON file {json_file_name} not found.")
    except Exception as e:
        logging.info("An error occurred in reading model parameters from metaData.json:" + e)
    logging.debug(f"{result_dict}")
    return result_dict 
def get_filename_and_extension(url):
    parsed_url = urlparse(url)
    path = parsed_url.path
    filename = Path(path).name
    return filename
def downloadUrlParameter(filePath, url):
    response = requests.get(url)
    content = response.content
    # get filename from url
    filename = get_filename_and_extension(url)
    # Save the content to a file or process it further
    # For example, save it to a file
    with open(filePath + "/" + filename, "wb") as file:
        file.write(content)


def get_output_parameters(filePath):
    # returns output parameters from metaData.json file 
    
    json_file_name = filePath + '/metaData.json'
    try:
        with open(json_file_name, 'r') as json_file:
            data = json.load(json_file)
        params_lst=data['modelMath']['parameter']
        result_list = [item["id"] for item in params_lst if item["classification"] == "OUTPUT" and item["dataType"] != "FILE"]
        logging.debug(f"Output parameters are {result_list}")
    except FileNotFoundError:
       logging.info(f"JSON file {json_file_name} not found.")
    except Exception as e:
        logging.info("An error occurred in reading model parameters from metaData.json:" + e)
    return result_list 


def save_output_vars_python():
    fskx_path = "/fskx"
    result_file = f"{output_path}/output.json"
    output_params = get_output_parameters(fskx_path)
    save_vars_code = f"import json\n\n"
    save_vars_code += f"import numpy as np\n\n"
    save_vars_code += f"import pandas as pd\n\n"
    save_vars_code += "variables_data = {}\n"
    
    for var_name in output_params:
        save_vars_code += f"variables_data['{var_name}'] = {var_name}.to_json(orient='table') if isinstance({var_name}, pd.DataFrame) else {var_name}.tolist() if isinstance({var_name}, np.ndarray) else {var_name} \n"
    
    save_vars_code += f"\nwith open('{result_file}', 'w') as json_file:\n"
    save_vars_code += "    json.dump(variables_data, json_file)\n"
    return save_vars_code


def save_plot_code_python(plot_type):
    logging.debug(f"Plot type is {plot_type}")
    plot_code = ""
    if plot_type=="png":
        result_file = f"{output_path}/plot.png"
        plot_code = f"\nplt.gcf().savefig('{result_file}', format='png')"
    elif plot_type=="svg":
        result_file = f"{output_path}/plot.svg"
        plot_code = f"\nplt.gcf().savefig('{result_file}', format='svg')"
    return plot_code


def save_output_vars_r():
    fskx_path = "/fskx"
    output_params = get_output_parameters(fskx_path)
    result_file = f"{output_path}/output.json"

    json_code = f"""
library(jsonlite)
var_list <- list({', '.join(repr(var) for var in output_params)})
var_values <- list()
"""
    for name in output_params:
        json_code += f"{name} = get('{name}')\n"
        json_code += f"var_values <- c(var_values, list({name} = {name}))\n"

    json_code += f"""
write_json(var_values, '{result_file}')
"""
    return json_code

def save_plot_code_r(plot_type):
    path = f"{output_path}"
    if plot_type=="png":
        result_file = f"{path}/plot.png"
        plot_code = f"png('{result_file}', width = 1000 , height = 1000)\n"
    elif plot_type=="svg":
        result_file = f"{path}/plot.svg"
        plot_code = f"svg('{result_file}')\n"
    elif plot_type=="pdf":
        result_file = f"{path}/plot.pdf"
        plot_code = f"pdf('{result_file}')\n"
    return plot_code

import logging
import containermanagement as cm
import json, os

modelLocation = "/fskx"

def run_simulation(config):

    if not os.path.exists(modelLocation):
        os.makedirs(modelLocation) 
    try:
        cm.run_simulation(modelLocation, config)
    except Exception as e:
        return json.dumps(e)
    return
    

def get_model_parameters():
    # get default parameter of simulation (not actively used after removing webserver)
    response = cm.get_model_parameters(modelLocation)
    return json.dumps(response)


def read_config_file():
    with open('/workspace/config') as f:
        data = json.load(f)
    return data


def main():
    try:
        config = read_config_file()
    except Exception as e:
        logging.error("Error reading config file")
        config = {}
    if config.get("command", "simulate") == "simulate":
        run_simulation(config)
    elif config.get("command") == "get-default-parameters":
        get_model_parameters()


if __name__ == "__main__":
    main()
"""Reads an extracted FSKX archive (metaData.json, packages.json, scripts).

Ported from fskx-container-image-management-service/FskxFile.py. Behaviour is
unchanged; only the Flask logger was replaced by the standard logging module.
Needs Python >= 3.10 (sys.stdlib_module_names).
"""
import ast
import json
import logging
import os
import re
import sys

logger = logging.getLogger(__name__)


class FskxFile:

    def __init__(self, fskx_file_path):
        self.fskx_file_path = fskx_file_path
        self.get_model_metadata()
        self.identifiy_programming_language()
        self.get_dependencies()

    def identifiy_programming_language(self):
        programming_language = self.general_information.get("languageWrittenIn", "r:3")
        if "python" in programming_language.lower():
            logger.debug(f"Identified programming language as Python from field '{programming_language}'.")
            self.programming_language = ProgrammingLanguage.python
        elif "r" in programming_language.lower():
            logger.debug(f"Identified programming language as R from field '{programming_language}'.")
            self.programming_language = ProgrammingLanguage.r
        else:
            logger.error("Could not identify programming language from metadata.json file.")
            self.programming_language = ProgrammingLanguage.error
            self.programming_version = 0
        pattern = r'(\d+(?:\.\d+)*)'
        match = re.search(pattern, programming_language)
        self.programming_version = match.group(1) if match else "3"
        logger.debug(f"Identified version of programming language as {self.programming_version}.")

    def get_model_metadata(self):
        metadata_file_name = "/metaData.json"
        with open(self.fskx_file_path + metadata_file_name) as metadata_file:
            metadata = json.load(metadata_file)

        self.metadata = metadata
        self.general_information = metadata["generalInformation"]
        self.model_name = metadata["generalInformation"]["name"]
        self.model_id = metadata["generalInformation"]["identifier"]

    def get_dependencies(self):
        packages_file_name = "/packages.json"
        with open(self.fskx_file_path + packages_file_name) as packages_file:
            packages_data = json.load(packages_file)
        dependencies = packages_data["PackageList"]
        self.extract_dependencies(dependencies)
        if not self.dependencies:
            logger.debug("No packages found in packages.json. Scanning scripts.")
            self.scan_scripts_for_dependencies()

    def extract_dependencies(self, packages):
        dependencies = {}
        for package in packages:
            package_name = package.get("Package", None)
            package_version = package.get("Version", None)
            if package_name and not self.is_standard_module(package_name):
                dependencies[package_name] = package_version
        self.dependencies = dependencies
        logger.debug(f"Found the following dependencies: {self.dependencies}.")

    def is_standard_module(self, module_name):
        if self.programming_language == ProgrammingLanguage.python:
            return self.is_standard_python_module(module_name)
        elif self.programming_language == ProgrammingLanguage.r:
            return self.is_r_base_package(module_name)

    def scan_scripts_for_dependencies(self):
        if self.programming_language == ProgrammingLanguage.python:
            self.get_imported_libraries_from_python_scripts()
        elif self.programming_language == ProgrammingLanguage.r:
            self.get_imported_libraries_from_r_scripts()
        logger.debug(f"Found the following dependencies: {self.dependencies}.")

    def is_standard_python_module(self, module_name):
        return module_name in set(sys.stdlib_module_names)

    def is_r_base_package(self, lib):
        base_packages = [
            "base", "compiler", "datasets", "graphics", "grDevices",
            "grid", "methods", "parallel", "splines", "stats",
            "stats4", "tcltk", "tools", "utils"
        ]
        if lib in base_packages:
            logger.debug(f"Package {lib} is already included as base package. Skipping installation.")
            return True
        return False

    def get_imports(self, filepath):
        with open(filepath) as f:
            root = ast.parse(f.read(), filepath)
        for node in ast.iter_child_nodes(root):
            if isinstance(node, ast.Import):
                for alias in node.names:
                    yield alias.name.split('.')[0]
            elif isinstance(node, ast.ImportFrom):
                yield node.module.split('.')[0]

    def get_imported_libraries_from_python_scripts(self):
        local_modules = set()
        for dirpath, _, filenames in os.walk(self.fskx_file_path):
            for filename in filenames:
                filepath = os.path.join(dirpath, filename)
                if filename.endswith(".py"):
                    logger.debug(f"Scanning file {filename} for dependencies.")
                    local_modules.add(filename[:-3])
                    for module in self.get_imports(filepath):
                        module = self.translate__python_import_to_dependency(module)
                        module = module.replace("'", '')
                        if not (module in local_modules or self.is_standard_python_module(module)):
                            logger.debug(f"Found library {module} in script.")
                            self.dependencies[module] = None
        if 'pandas' in self.dependencies.keys():
            self.dependencies['openpyxl'] = None

    def get_imported_libraries_from_r_scripts(self):
        for dirpath, _, filenames in os.walk(self.fskx_file_path):
            for filename in filenames:
                filepath = os.path.join(dirpath, filename)
                if filename.endswith(".r"):
                    logger.debug(f"Scanning file {filename} for dependencies.")
                    with open(filepath, 'r') as f:
                        script_content = f.read()
                    for line in script_content.splitlines():
                        match = re.search(r"(?:library|require)(\(['\"]|\()(.*?)(['\"]\)|\))", line)
                        if match:
                            library_name = match.group(2)
                            if not self.is_r_base_package(library_name):
                                logger.debug(f"Found library {library_name} in script.")
                                self.dependencies[library_name] = None

    def translate__python_import_to_dependency(self, module):
        translations = {
            "sklearn": "scikit-learn",
            "PIL": "pillow",
            "bs4": "beautifulsoup4",
            "skimage": "scikit-image",
            "openssl": "pyOpenSSL",
            "Crypto": "pycrypto",
            "magic": "python-magic",
            "cv2": "opencv-python-headless",
        }
        if module in translations.keys():
            return translations[module]
        return module


class ProgrammingLanguage():
    python = "Python"
    r = "R"
    error = "Error"

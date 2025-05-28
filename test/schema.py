import inspect
import os
import sys

import json
import jsonschema


def validate_json_outline_string (json_string):
    with open(os.path.join(os.path.dirname(inspect.stack()[-1].filename),
                           "outline-schema-reverse-engineered.json")) as f:
        schema = json.load(f)
    jsonschema.validate(instance=json.loads(json_string), schema=schema)
    return True


def validate_json_outline_file (path):
    with open(path) as f:
        data = f.read()
    return validate_json_outline_string(data)


if __name__ == "__main__" and os.getenv("R_SESSION_INITIALIZED") is None:
    try:
        validate_json_outline_file(sys.argv[-1])
        print("✔ JSON is valid.")
    except jsonschema.ValidationError as e:
        print("❌ JSON validation error:")
        print(e)

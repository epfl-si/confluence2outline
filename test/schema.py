import os
import sys

import json
import jsonschema

def validate_json_outline (json_string):
    with open(os.path.join(os.path.dirname(__file__),
                           "outline-schema-reverse-engineered.json")) as f:
        schema = json.load(f)
    jsonschema.validate(instance=json.loads(json_string), schema=schema)

if __name__ == "__main__" and os.getenv("R_SESSION_INITIALIZED") is None:
    with open(sys.argv[-1]) as f:
        data = f.read()

    try:
        validate_json_outline(data)
        print("✔ JSON is valid.")
    except jsonschema.ValidationError as e:
        print("❌ JSON validation error:")
        print(e)

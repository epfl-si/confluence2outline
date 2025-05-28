import os
import sys

import json
import jsonschema

if __name__ == "__main__" and os.getenv("R_SESSION_INITIALIZED") is None:
    with open(sys.argv[-2]) as f:
        data = json.load(f)

    with open(sys.argv[-1]) as f:
        schema = json.load(f)

    try:
        jsonschema.validate(instance=data, schema=schema)
        print("✔ JSON is valid.")
    except jsonschema.ValidationError as e:
        print("❌ JSON validation error:")
        print(e)

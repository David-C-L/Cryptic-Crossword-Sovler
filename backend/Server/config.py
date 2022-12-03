import os
from dotenv import load_dotenv

load_dotenv()


class Config:
    SOLVER = os.environ.get("SOLVER", "http://146.169.42.182")
    DUMMY = os.environ.get("DUMMY")
    PRODUCTION = int(os.environ.get("PRODUCTION") if os.environ.get("PRODUCTION") is not None else 0) == 1

import pymongo

CLIENT: pymongo.MongoClient = pymongo.MongoClient("localhost", 27017)
PDA_DB = CLIENT.pda
DESIGNS = PDA_DB.designs

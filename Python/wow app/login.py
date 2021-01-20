import requests
import json

client_id = "86d4174c00ab47dd821410a7476f7232"
client_secret = "X5Guj3ghYzMIsijNUGqmYJmXKAFhHeas"

uri_token = "https://eu.battle.net/oauth/token"
uri_api = "https://eu.api.blizzard.com/"

p_grant_type = "grant_type=client_credentials"

p_ns = "namespace=dynamic-eu"
p_locale = "locale=en_GB"

resp = requests.get(uri_token+'?'+p_grant_type, auth=(client_id,client_secret))

token = resp.json()["access_token"]

print(token)

params = '?'+p_ns+'&'+p_locale+'&'+'access_token='+token

api_connected_realm_index = "/data/wow/connected-realm/index"

con_realms = requests.get(uri_api+api_connected_realm_index+params)

print (con_realms.json())



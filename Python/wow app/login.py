# built in modules
import json
# downloaded packages.  use 'pip install requests'
import requests
# projet modules (none yet)


class CwowAPI:
  def __init__( self, client_id, client_secret, domain, locale ):

# connect to wow and get a token
    
# domain should be eu
    uri_token = "https://" + domain + ".battle.net/oauth/token"
    
    LoginParams = {}
    LoginParams["grant_type"]="client_credentials"

#    grant_type = "grant_type=client_credentials"

#    p_ns = "namespace=dynamic-eu"
    self.ns = "namespace=dynamic-" + domain
    
    self.locale = "locale=" + locale #en_GB"

#    resp = requests.get(uri_token+'?'+p_grant_type, auth=(client_id,client_secret))
    resp = requests.get(formatRequest(uri_token,LoginParams), auth=(client_id,client_secret))
    
    print("get token response is ", resp)

    self.token = resp.json()["access_token"]
    
    self.MainParams = {}
    self.MainParams["locale"]=locale
    self.MainParams["namespace"]="dynamic-" + domain
    self.MainParams["access_token"] = self.token

    print("token is \n",self.token)
    
# set up other values    
    self.uri_api = "https://" + domain + ".api.blizzard.com/"

  def getData( self, api, params ):
    api_connected_realm_index = "/data/wow/connected-realm/index"
#    con_realms = requests.get(formatRequest(uri_api+api_connected_realm_index,self.MainParams))
    ret = requests.get(formatRequest(self.uri_api+api_connected_realm_index,self.MainParams))
    print(ret)
    return ret
    
def formatRequest( uri, params ):
  ret = uri
#  if len(params)>0:
#    ret += "?"
  delim = "?"
  for n, v in params.items():
    ret += "{}{}={}".format(delim,n,v)
    delim = "&"
  print("formatRequest returns",ret)
  return ret



#params = '?'+p_ns+'&'+p_locale+'&'+'access_token='+token
client_id = "86d4174c00ab47dd821410a7476f7232"
client_secret = "X5Guj3ghYzMIsijNUGqmYJmXKAFhHeas"
wowAPI = CwowAPI( client_id, client_secret, "eu", "en_GB" )

api_connected_realm_index = "/data/wow/connected-realm/index"

con_realms = wowAPI.getData( api_connected_realm_index, {} )

print (con_realms.json())



# built in modules
import json, itertools
from pprint import pprint 
# downloaded packages.  use 'pip install requests'
import requests
# projet modules (none yet)

apiIndex = {}
apiIndex["connected-realm"] = "data/wow/connected-realm/index"
apiIndex["realm"]="data/wow/realm/index"


class CwowAPI:
  def __init__( self, client_id, client_secret, domain, locale ):

# connect to wow and get a token
    uri_token = "https://" + domain + ".battle.net/oauth/token"
    
    LoginParams = {}
    LoginParams["grant_type"]="client_credentials"
    self.ns = "namespace=dynamic-" + domain     #eu
    self.locale = "locale=" + locale            #en_GB"

    resp = requests.get("{}{}".format(uri_token,formatParams(LoginParams, "na")), auth=(client_id,client_secret))
    self.token = resp.json()["access_token"]
    
    MainParams = {}
    MainParams["locale"]=locale
    MainParams["access_token"] = self.token
    MainParams["namespace"]=domain  #"dynamic-" + domain
    self.apiParamsDyn = formatParams( MainParams, "{}{}=dynamic-{}" )
    self.apiParamsStatic = formatParams( MainParams, "{}{}=static-{}" )
    self.apiParamsMin = formatParams( MainParams, "" )

    print("token is \n",self.token)
    
# set up other values    
    self.uri_api = "https://" + domain + ".api.blizzard.com/data/wow/"

  def getData( self, api, rootname ):
    rqst = "{}{}/index{}".format(self.uri_api,api,self.apiParamsDyn)
    print( "rqst =",rqst)
    print( "rootname =",rootname )
    ret = requests.get(rqst)
    print("getData ret=",ret)
    return ret.json()[rootname]
    
  def getValue( self, api, rootname, keyname, keyvalue, value_name  ):
    ret = requests.get("{}{}/index{}".format(self.uri_api,api,self.apiParamsDyn))
    print("getValue ret=",ret)
    for d in ret.json()[rootname]:
      if d[keyname] == keyvalue:
        return d[value_name]
        
  def getDetails( self, api, keyvalue ):
    rqst = "{}{}/{}{}".format(self.uri_api,api,keyvalue,self.apiParamsDyn)
    print( "rqst =",rqst)
    ret = requests.get(rqst)
    print("getDetails ret=",ret)
    pprint( ret.json() )
    return ret.json()
    

  def getRealmSlug( self, realm ):
    return self.getValue( "realm", "realms", "name", realm, "slug" )
    
  def getConnectedRealmAuction( self, realm ):
    slug = self.getRealmSlug( realm )
    realm_details = self.getDetails( "realm", slug )
    con_realm_link = realm_details["connected_realm"]["href"]
    print( con_realm_link )
    rqst = "{}{}".format( con_realm_link, self.apiParamsMin )
    ret = requests.get( rqst )
    auction_link = ret.json()["auctions"]["href"]
    rqst = "{}{}".format( auction_link, self.apiParamsMin )
    ret = requests.get( rqst )
    print( "getConnectedRealm ret=", ret )
    return ret.json()["auctions"]
    
  def getItems( self ):
    rqst = "{}{}/index{}".format(self.uri_api,"item-class",self.apiParamsStatic)
    print( "rqst =",rqst)
    ret = requests.get(rqst)
    print("getItems ret=",ret)
    classes = ret.json()["item_classes"]
    for rec in classes:
      class_name = rec["name"]
      class_id = rec["id"]
      class_rqst = "{}{}".format( rec["key"]["href"], self.apiParamsMin )
      class_ret = requests.get( class_rqst )
      
    return ret.json()
    
  def describeRecords( self, api, rootname ):
    data = self.getData( api, rootname )
    for rec in data[:3]:
      pprint( rec )
      for n, v in rec.items():
        print( n )
    return

  def describeData( self, api ):
    d = requests.get("{}{}/index{}".format(self.uri_api,api,self.apiParamsDyn)).json()
    for n, v in d.items():
      print( n )
    return

# def formatParams( params ):
  # ret = ""
  # delim = "?"
  # for n, v in params.items():
    # ret += "{}{}={}".format(delim,n,v)
    # delim = "&"
  # return ret

# def formatParamsMin( params ):
  # ret = ""
  # delim = "&"
  # for n, v in params.items():
    # if n != "namespace":
      # ret += "{}{}={}".format(delim,n,v)
  # return ret

def formatParams( params, ns_format ):
  ret = ""
  if ns_format == "":
    delim = "&"
  else:
    delim = "?"
  for n, v in params.items():
    if n != "namespace":
      ret += "{}{}={}".format(delim,n,v)
    else:
      ret += ns_format.format(delim,n,v)
    delim = "&"
  return ret

client_id = "86d4174c00ab47dd821410a7476f7232"
client_secret = "X5Guj3ghYzMIsijNUGqmYJmXKAFhHeas"
wowAPI = CwowAPI( client_id, client_secret, "eu", "en_GB" )

api_connected_realm_index = "/data/wow/connected-realm/index"

wowAPI.describeData( "connected-realm" )
wowAPI.describeRecords( "connected-realm", "connected_realms" )
wowAPI.describeData( "realm" )
wowAPI.describeRecords( "realm", "realms" )
wowAPI.getDetails( "realm", "aszune" )
print( wowAPI.getRealmSlug( "Aszune" ) )

d = wowAPI.getConnectedRealmAuction( "Aszune" )

with open('file.txt', 'w') as file:
     file.write(json.dumps(d))

for rec in d[:2]:
  pprint( rec )
  
  
pprint( wowAPI.getItems() )
#pprint(realms.json())
#pprint( dict(itertools.islice(realms.json().items(), 3)))

#import itertools

#d = {1: 2, 3: 4, 5: 6}

#dict(itertools.islice(d.items(), 2))

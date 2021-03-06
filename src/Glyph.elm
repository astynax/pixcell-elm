module Glyph exposing (..)


resize : String
resize =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKESgMokPVlwAAAGBJREFUOMu1U0EOwDAIkn2cp7PL2jQdaTRuXAUSUCFJsQEAwsBxQw9IKpIgOWSKIc6arNxpsA8y4hlpzXUycTPbiSNWIr4EZbFr+iS+4g+0IpRLbK/xk0Nqn3LnmdB95xuueymswfvSzAAAAABJRU5ErkJggg=="


grid : String
grid =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKESgl4PFN+wAAACVJREFUKM9jYEAC/////08Kn4mBAkCRZkZ0p5ANRv086ufB5WcAruI72JJOEgoAAAAASUVORK5CYII="


floppy : String
floppy =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKESc0DdlxxgAAAExJREFUOMvNkTEOACAIAz3+/+e6OBgHjCkkdoGlpaVI0nDgCEhSvBw5Z4kD3B+EQwZIHQDcYsaNfO6lEVKB3XYas7UFFtpq/EPAfuIEWbo79B1n8kUAAAAASUVORK5CYII="


undo : String
undo =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKESkYoYIwqwAAAEhJREFUOMtjYKAFqK+v/48O6uvr/xOlGaaBWHEMRfhsgrmMdEliLCFGMza1TJQGOBOyk4j1BjJgRHcSIyMjI1lOIdXmUTCYAADByVuip7rqxQAAAABJRU5ErkJggg=="


redo : String
redo =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKESkx4zCoxwAAAEpJREFUOMtjYKAFqK+v/48O6uvr/xOlGaaBWHEMRfhsgrmMdEliLCFGMza1TJQGOEkGIDsf5mVGUm1Edj4jIyMjWc4mJbxGAT0AACkMW6JN6f9SAAAAAElFTkSuQmCC"


paint : String
paint =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEgcSSBZuwAAAAGJJREFUOMvFkTEOwDAIA00+7vz8OkViQKVJht4CihQbjCQJYKeWnIpEq1wQEbH6AZAfvpANx2rmnNoVsU2sCU7WALjKYPffaxYltnnDNncO+QqVe67H+3VTDF3yv0B7wi7EB694loR84QrxAAAAAElFTkSuQmCC"


scrollR : String
scrollR =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEhcXcr6IHgAAAGxJREFUOMvFkUESwCAIAxOn/8afpycdtOpAL+4RSQgC3IYAIEkAUGsNiVofSXaxNzox99MXSDKSwGuKj6Qgw7qR2KckJRt5pmSmrepPZpokzR/9MVhdopmu3sIrbE9sZvpzCUkyMw0mGbr4Oi+3LaeD0aIHQAAAAABJRU5ErkJggg=="


scrollD : String
scrollD =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEhc1p97J+gAAAGhJREFUOMvdU0EOwCAIK2T/hp93JxJihLF5Wy+WAA1aBBI4gJkRO5BknGVRl88CwTuR4BrE3UGSIiJ4Ac1B1dxNpDjEDwSuqV3B14duJ1iLdy49XiGaKot14vdouY5XOcPd2+Yy/+U73/Emmh0RFDn3AAAAAElFTkSuQmCC"


flipH : String
flipH =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEhkCgeBBewAAAF1JREFUKM+1UUEOwDAIgmX/xp/Ty7o02WpjunEUFVAggW1nPKsDAECSZaUp/0aMtaWjqvJ3mauLDmxga/gcLc6yPA519d3KETHNSJIRkduQ5AydX+bpjZJcfeX/r2qFK3CvViPl+gAAAABJRU5ErkJggg=="


flipV : String
flipV =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEhkpLVy4OwAAAFZJREFUKM/Nk0sOwCAIRB0vztGfKxNCaxVdWDYEkvkAoZRfBMCob2ZMgWmCCPgi8HU9GTMN9tbVrUgSQM/bW04tbQYeXaKuKMZZH8qe/U15+86EuP8LDRU+inwfsWIwAAAAAElFTkSuQmCC"


rotate : String
rotate =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA8AAAAPCAYAAAA71pVKAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEhoVKR6XfwAAAFlJREFUKM/NUVsOgDAIW433LjevXyRTx2BZYuwfBVoerf0SkrTVuCzwbCgLRIUjHlEBAGR8ad9omnOmnLkcfWBmN3eHcyT1mkKShomVv7sASVV4lN+SXflTXEYgbCkGgWWSAAAAAElFTkSuQmCC"


reflectQ : String
reflectQ =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKExAp/VxpRQAAAH5JREFUOMudUUkSgDAMSjr9d/tzvFgHMYlVjmGxoAOAFXB3r/im4sqAE3zrKlhBKgSAKLxlX2JxZg4D5pxXb34JAIwxkPYKSeIZN60eNEiNjxAei8XV8nzr3FOHsw20NRzXiczKLY9x911kg78GlcboyV/3sMqwFRL9si/8LxzjXxgl10Q0MAAAAABJRU5ErkJggg=="


reflectH : String
reflectH =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKExELMScZ4AAAAIhJREFUOMutU1sOwDAIQi/e3pz9bIujskcy/xpRAW2QJG4iIuIunwAw53ws0kFjDC4Jlqg5La6YdJPPCcJwkUySFawTHMOLhM7IToZKBIC4PF6Yp7jsdD5FlXwycPt2d3LgUzs7T2KPhama4zbS5f410V2cNtV3Ooe7S+w2FY6ibqdjYxt8+c4bSwzdc1ZQfsEAAAAASUVORK5CYII="


reflectV : String
reflectV =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKExE18EYESwAAAGJJREFUOMvlkkESwCAIA4MfDz9PL3bGomg7euseQQJETJKwQcEmZwSs4u7fFVQhqRh7w+NxFG5FAYDkXCD7kZhrhYYrjIqzKcttYLa7BWJu2bHtupq2MzSalhZ2xpy4hR9xAdsPEZ3WauKkAAAAAElFTkSuQmCC"


reflectR : String
reflectR =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKExMGfaAH3wAAAJNJREFUOMudU0EOwCAIa/04/JydWBoEzdaLEwatRREDzCwAQL9bTIVdvmvEiHiDJNkRbEXy36pBZZwaa24BgLvjpiLjub7HqRJzr2tVpPul7B3TSZmZBXFBZ+I4hT9Y1cDEyCg5dwdVpo6yFqU/dbzbGNXIromyAwAns7RAG9YYvzh+vMqZ0ONUuDtOBm+X5PQKEw/Qfs+rn+hsmgAAAABJRU5ErkJggg=="


cycle : String
cycle =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEywY33EQgAAAAKVJREFUOMutU8ENxCAMs6tbMfPAPhnS9yggC9prdcIfSBrbkAbgByQJD/j0TVhxkgSAWiskiS1WxKhhJodAXDi5u4uMXISYyWMmdneSrLWi7911QUh6OsXs7tfZgzcdv6o9PDGLqMHjUooAoJRyfutJX+8EPLec+N8r7Grg+SPfurVyjSa60Ezuw+Rkj3lFJJOzM0ku5OQ5yuQ6orQHNb+DTt7S8S+7VbZGP5h1zQAAAABJRU5ErkJggg=="


palette : String
palette =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH5AwKEywticLUowAAAKRJREFUOMu1UsERgzAMk7iu6HkykIcUn5gKA4W7tvok8UWK7IhSCAYyCQAx6znPAKCQAIDJrYYu4GRHkfv+5SK0185cOLGwFLGTu/2yvrPfHZRYMnk2g/9COvb35O7ihTsRSRpjCADGGJKkrejrnUjhJy18jRact3KSMzz2xZlUhGrdcuDkIjqZeZ2FjyHx193BIcrlIC6G4w6Yyd356Qx6G1VfAce8pcshDwm1AAAAAElFTkSuQmCC"

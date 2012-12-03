REBAR=rebar
PWD=`pwd`
PROJECT="erchord"


all: clean compile xref eunit                                                   

app:
	@$(REBAR) create template=skel dest=$(PWD) src=$(PWD)/src appid="'$(PROJECT)'" skip_deps=true

compile:
	@rebar compile

xref:
	@rebar xref

clean:
	@rebar clean

eunit:
	@rebar eunit

edoc:
	@rebar doc

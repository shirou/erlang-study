REBAR=rebar
PWD=`pwd`


all: clean compile xref eunit                                                   

app:
	@$(REBAR) create template=skel dest=$(pWD) src=$(PWD)/src appid="'$(PROJECT)'" skip_deps=true

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
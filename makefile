.PHONY: all

all: eventstore

eventstore:
	docker run --rm --name eventstore-node -it -p 2113:2113 -p 1113:1113 \
	-e EVENTSTORE_RUN_PROJECTIONS=All \
	-e EVENTSTORE_START_STANDARD_PROJECTIONS=true \
	eventstore/eventstore

shell:
	rebar3 shell

#!/bin/bash

cd `dirname $0`

CORE_HOME=`(cd ../cores && echo $PWD)`

#JAVA_OPTS="-Dsolr.solr.home=$CORE_HOME"
JAVA_OPTS="-Dsolr.solr.home=$CORE_HOME -Djava.util.logging.config.file=etc/logging.properties"

exec java $JAVA_OPTS -jar start.jar

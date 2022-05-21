#!/bin/sh
if [ ! $# == 2 ]; then
	echo "Usage: adedb.sh <instancename> <command>"
	echo "Example: adedb.sh contentDB [create|destroy|start|stop]"	
	exit
fi
# example: sitesdb [Note:maximum of seven characters]
viewname="$1"
# example: RDBMS_11.1.0.7.0_LINUX_RELEASE
labelName="RDBMS_11.1.0.7.0_LINUX_RELEASE"
# example: 1521
port=1521

LEN=$(echo ${#viewname}) 
if [ $LEN -gt 7 ]; then
        echo "$viewname cannot exceed 7 characters"
        exit;
fi


# example: create, start, stop, destroy
if [ "$2" == "create" ]; then
	echo "Creating view $viewname..."
	ade createview $viewname -label $labelName -force
	ade useview $viewname -exec "$(pwd)/dblistener.sh $viewname $port $USER"	
	ade useview $viewname -exec "lsnrctl start"
	echo "Default login is sys/knl_test7 and service is $viewname.regress.rdbms.dev.us.oracle.com"	
else 
	echo "Executing $2 command on Database $viewname"
	if [ "$2" == "destroy" ]; then
		ade destroyview $viewname
	else
		ade useview $viewname 
		ade useview $viewname -exec "lsnrctl $2"
	fi
fi


clear
echo "Which rocket would you like to run: "
read rocket_type
if [ $rocket_type == "A8-3" ]
then
	echo "Switching to A8-3 Engine Type"
	cd data/
	cp mass-A8-3.csv mass.csv
	cp thrust-A8-3.csv thrust.csv
	cd ..
elif [ $rocket_type == "B6-4" ]
then
	echo "Switching to B6-4 Engine Type"
	cd data/
	cp mass-B6-4.csv mass.csv
	cp thrust-B6-4.csv thrust.csv
	cd ..
else
	echo "No engine found on record"
fi

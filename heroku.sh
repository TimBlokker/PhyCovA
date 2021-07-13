#deployment script for heroku

#cd ing into treedist directory
#cd /home/tim/Documents/Bioinformatics/MasterThesis/treeDist

#login into heroku containers
heroku container:login

#build the container
#!!!!!make sure you ran renv::init() in hte R console before
#docker build -t registry.heroku.com/gentle-taiga-91395/web .

#push to heroku
docker push registry.heroku.com/gentle-taiga-91395/web

#release the container
heroku container:release -a gentle-taiga-91395 web

#open the application to see if it works
heroku open -a container:release -a gentle-taiga-91395

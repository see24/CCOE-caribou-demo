# Running analysis in the cloud

We had trouble with running in parallel on one VM seemingly related to uncaught errors or failures. So we are setting up a pool with many small VMs. Using DS2_v3 because had memory issues with small ones. 

Steps:

1) Make sure the Caribou-Demographic-Projection-Paper is correct and has been pushed to GitHub. If doing a new set should just need to change in sensitivityMinimal
3) In make_batch_scripts, make sure that the SASURL for the Azure storage container is up-to-date. Then create 24 shell scripts and task json files as well as a pool json file.
4) Move the 24 run_caribou.sh scripts to the Azure storage container
5) Use the caribou_add_pool1.json file to create a new pool with 24 nodes. You need to fill in the pool ID by hand
6) Add a job to the pool
7) Add a task using the json files for all 24 caribouDemo.json files. 

Tasks will run and when complete the results should be copied to the Azure storage container. This should take ~2 days. 
When complete ensure that all results are successfully copied and then delete the pool. We will continue paying until the pool is deleted to try to do it ASAP after completion

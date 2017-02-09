# The API

The API provided by this service is divided into two parts. The first faces
clients, who want to queue up trancode jobs and query their status. The second
faces workers, who dequeue jobs, occasionally send out updates, and hopefully
eventually finish the job.

## Client API

Clients get the following API:

 * Queue a job: POST request to /jobs with the following JSON object (more
   details on what the object fields can be are listed further below), which in
   turn returns the job ID of the newly created job:
```
   {
     "input-uri": "http://uris/can/be/anything/gstreamer/understands.mp4"
     "output-uri": "file:///like/a/path/to/a/file.mkv",
     "profile": "YouTube",
     ...
   }
```

 * Query a job: GET request on /jobs/id will return a JSON object as above,
   with the various job parameters and the current job state.

 * Cancel a job: DELETE request on /jobs/id will cause the job to be cancelled.

 * Query all jobs: GET request on /jobs will return all jobs (FIXME: this
   should return incomplete jobs, perhaps?). Each of jobis also represented by
   a JSON object as in the queue 

### The Job object

The job object that is passed to the POST request, or queried by GET requests,
is essentially the same. The only difference is that at the time of the POST
request, the "id" field will not be present. The relevant fields are:

 * input-uri
 * output-uri
 * profile
   * target
   * name
   * category
 * container
 * video-params
   * codec
   * width
   * height
   * framerate-n
   * framerate-d
   * bitrate
   * extra
   * preset
 * audio-params
   * codec
   * rate
   * channels
   * bit depth
   * birate
   * extra
   * preset

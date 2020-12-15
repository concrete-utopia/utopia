const fs = require('fs')
const path = require('path')
const AWS = require('aws-sdk')

AWS.config.update({ region: 'eu-west-2',"accessKeyId": "AKIAJQXJFEFKZZOS4S6A", "secretAccessKey": "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" })



const testFile = "TestPic.png"
const metaData = testFile.split('.').pop();
let s3 = new AWS.S3({apiVersion: '2006-03-01'});
let file = testFile;
const uploadParams = {Bucket: "frame-test-png", Key: testFile, Body: '', ContentType: 'image/png' , ACL: 'public-read'};

let filestream = fs.createReadStream(file);
filestream.on('error', function(err) {
    console.log('File Error', err);
});
uploadParams.Body = filestream;
uploadParams.Key = path.basename(file);

s3.upload(uploadParams, function (err, data) {
    if (err) {
    console.log("Error", err);
    } if (data) {
    console.log("Upload Success", data.Location)
    }
});
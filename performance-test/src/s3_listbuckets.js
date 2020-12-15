let AWS = require('aws-sdk');
AWS.config.update({
                region: 'eu-west-2',
                "accessKeyId": "AKIAZWDT7ZEUVDBH7AMH",
                "secretAccessKey": "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" })


let s3 = new AWS.S3({apiVersion: '2006-03-01'});

s3.listBuckets(function(err, data) {
    if (err) {
        console.log("Error", err);
    } else {
        console.log("Success", data.Buckets);
    }
});
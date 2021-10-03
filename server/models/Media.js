const mongoose = require('mongoose');
const mediaSchema = new mongoose.Schema({
    mediaName: {
        type: String,
        required:true,
        min:6,
        max:255
    },
    ipfsHash: {
        type: String,
        required:true,
        min:6,
        max:255
    },
    ipfsPath: {
        type: String,
        required:true,
        min:6,
        max:255
    },
    txId: String // upload transaction id
});

module.exports = mongoose.model('Media', mediaSchema);

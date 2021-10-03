const mongoose = require('mongoose');
const mediaOwnerSchema = new mongoose.Schema({
    mediaName: {
        type: String,
        required:true,
        min:6,
        max:255
    },
    onwers: [{userName: String, sharePercent: Number}],
    nft: {
        id: String,
        createDate: Date,
        txId: String
    }
});

module.exports = mongoose.model('MediaOnwer', mediaOwnerSchema);

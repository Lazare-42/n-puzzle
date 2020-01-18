// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// No Node.js APIs are available in this process because
// `nodeIntegration` is turned off. Use `preload.js` to
// selectively enable features needed in the rendering
// process.

// Backend and endpoint details.
const host     = 'http://127.0.0.1:8080'
const endpoint = '/users'
// Retry configuration.
let maxNoOfAttempts        = 50,
    waitTimeBetweenAttempt = 250


let _fetchUserList = function(waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpoint, function(users) {
    $('#status').html(`Fetched the content after attemt no.
                       ${currentAttemptNo}!`)
    // Construct the user list HTML output
    let output = "";
    for (let i in users) {
      let user = users[i]
      output += `ID: ${user.userId},
                 Firstname: ${user.userFirstName},
                 Lastname: ${user.userLastName}
                 <br>`
    }
    $('#userList').html(output)
  }).fail(function() {
    $('#status').html(`Attempt no. <b>${currentAttemptNo}</b>. Are you sure the
                       server is running on <b>${host}</b>, and the endpoint
                       <b>${endpoint}</b> is correct?`)
    // Keep trying until we get an answer or reach the maximum number of retries.
    if (currentAttemptNo < maxAttempts) {
      setTimeout(function() {
        _fetchUserList(waitTime, maxAttempts, currentAttemptNo+1)
      }, waitTime)
    }
  })
}

// Convenience function for `_fetchUserList`.
let fetchUserList = function(waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}

// Start trying to fetch the user list.
fetchUserList(waitTimeBetweenAttempt, maxNoOfAttempts)

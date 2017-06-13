function loadComments(commentId) {
  var id = commentId;
  console.log("Id is " + id)
  return new Promise(function(resolve, reject) {
    let url = ["https://api.github.com/repos/farre/blog/issues", id, "comments"].join('/')
    var request = new XMLHttpRequest();
    request.open('GET', url);
    request.withCredentials = true;
    request.setRequestHeader("Accept", "application/vnd.github.full+json");
    request.responseType = 'json';
    request.onload = function() {
      if (request.status === 200) {
        resolve(request.response);
      } else {
        reject(Error('Failed to load: ' + request.statusText));
      }
    };
    request.onerror = function() {
      reject(Error('There was a network error'));
    };

    request.send();
  });
}

function populateComments(receiver) {
  if (!receiver) {
    return;
  }

  loadComments(receiver.dataset.id).then(data => {
    console.log(data);
  }).catch(error => {
    console.log(error);
  });
}

function commentHandler(event) {
  var comments = document.getElementById('comments');
  populateComments(comments);
}

addEventListener('load', commentHandler)

function loadComments(commentId) {
  var id = commentId;
  console.log("Id is " + id)
  return new Promise(function(resolve, reject) {
    let url = ["https://api.github.com/repos/farre/blog/issues", id, "comments"].join('/')
    var request = new XMLHttpRequest();
    request.open('GET', url);
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

  loadComments(receiver.dataset.id).then(comments => {
    var elementTemplate = document.getElementById('comment-template');
    var imageTemplate = document.getElementById('image-template');
    for (let comment of comments) {
      let user = comment.user;

      let element = elementTemplate.cloneNode(true);
      let image = imageTemplate.cloneNode(false);
      image.src = user.avatar_url;
      let name = element.querySelector("a.commentuser");
      name.href = user.html_url;
      name.innerText = user.login;
      let date = element.querySelector("a.commentdate");
      date.href = comment.html_url;
      date.innerText = (new Date(comment.created_at)).toUTCString();

      element.querySelector("div.commentgravatar").appendChild(image);
      element.querySelector("div.commentbody").innerHTML = comment.body_html;

      receiver.appendChild(element);
    }
  }).catch(error => {
    console.log(error);
  });
}

function commentHandler(event) {
  var comments = document.getElementById('comments');
  populateComments(comments);
}

addEventListener('load', commentHandler)

from flask import Flask, request, make_response

app = Flask(__name__)

@app.route("/set_cookie/<usr>")
def set_cookie(usr : str = "john doe"):
    response = make_response("cookie is set")
    response.set_cookie("username", usr, max_age=60*60*24) # 1 nap
    return response

@app.route("/get_cookie")
def get_cookie():
    username = request.cookies.get("usermane")
    return f"username: {username}" if username else "No cookie found"

if __name__ == "__main__":
    app.run(debug=True)

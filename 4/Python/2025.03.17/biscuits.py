from flask import Flask, render_template, request, make_response, redirect, url_for

app = Flask(__name__) 
@app.route('/') 
def index(): 
    return render_template('index.html') 

@app.route('/setcookie', methods = ['POST', 'GET']) 
def setcookie(): 
    if request.method == 'POST':
        try:
            user = request.form['nm']
            paswd = request.form['pw']
            if user == "":
                raise BaseException("No name was given") 
            # if paswd == "":
            #     raise BaseException("No passwd was given") 
            resp = make_response(render_template('cookie.html')) 
            resp.set_cookie('userID', user)
            resp.set_cookie('pwd', paswd) 
            return resp 
        except BaseException as e:
            return redirect(url_for('index'))

@app.route('/getcookie') 
def getcookie(): 
    name = request.cookies.get('userID') 
    return '<h1>welcome '+name+'</h1>'

# THis view gets the stored cookies
@app.route('/getcookies_html') 
def getcookies_html(): 
    name = request.cookies.get('userID')
    pwd = request.cookies.get('pwd') 
    return render_template('profile.html', username=name, password=pwd)

if __name__ == "__main__":
    app.run(debug=True)
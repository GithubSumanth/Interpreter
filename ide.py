import subprocess
from tkinter import *
from tkinter.filedialog import askopenfilename, asksaveasfilename
from tkinter import font
import interpreter
import webbrowser
import sys

# Creating a Window instance
window = Tk()
file_path = ""

# Setting the window size
window.geometry("1300x600")

# Setting background color to black
window.config(background="#EEEEEE")

# Setting title of the project
window.title("Interpreter")

font_family1 = font.Font(family='Courier New', size=12)
font_family2 = font.Font(family='Helvatica', size=12)

text_editor = Text(height=50,
                   width=70,
                   background="#00092C",
                   foreground="#eee",
                   insertbackground="#eee",
                   font=font_family1)
text_editor.insert('1.0', "{ Write your code here }")
text_editor.pack(side=LEFT)

output_area = Text(height=50,
                   width=70,
                   background="#000312",
                   foreground="#44e41c",
                   insertbackground="#eee",
                   font=font_family1)
output_area.pack(side=LEFT)

new_font = font.Font(font=text_editor['font'])
tab_size = new_font.measure("    ")
text_editor.config(tabs=tab_size)

def new_page_functionality():
    global file_path
    file_path = ""
    text_editor.delete('1.0', END)


def open_functionality(event=None):
    global file_path
    path = askopenfilename(filetypes=[('RVCE Files', '*.rvce')])
    with open(path, 'r') as file:
        code = file.read()
        text_editor.delete('1.0', END)
        text_editor.insert('1.0', code)
        file_path = path


def save_functionality(event=None):
    global file_path
    if file_path == "":
        path = asksaveasfilename(filetypes=[('RVCE Files', '*.rvce')])
        file_path = path
    else:
        path = file_path
    with open(path, 'w') as file:
        code = text_editor.get('1.0', END)
        code = code.strip()
        file.write(code)


def save_as_functionality(event=None):
    global file_path
    path = asksaveasfilename(filetypes=[('RVCE Files', '*.rvce')])
    with open(path, 'w') as file:
        code = text_editor.get('1.0', END)
        file.write(code)
        file_path = path


def set_dark_mode(event=None):
    text_editor.config(
        background="#00092C",
        foreground="#eee",
        insertbackground="#eee",
    )

    output_area.config(
        background="#000312",
        foreground="#44e41c",
        insertbackground="#eee",
    )


def set_light_mode(event=None):
    text_editor.config(
        background="#fff",
        foreground="#231955",
        insertbackground="#000",
    )

    output_area.config(
        background="#FBF8F1",
        foreground="#711A75",
        insertbackground="#000"
    )


def help_functionality(event=None):
    webbrowser.open("https://githubsumanth.github.io/Pascal-Documentation/")

def run_functionality(event=None):
    global file_path
    output_area.delete('1.0', END)
    text = text_editor.get('1.0', END)
    try:
        output = interpreter.main(text)
    except:
        output = sys.exc_info()
    output_area.insert('1.0', output)

menu_bar = Menu(window, background='blue', fg='white')

file_bar = Menu(menu_bar, tearoff=0)
file_bar.add_command(label="\tNew\t\t\t\t", command=new_page_functionality)
file_bar.add("separator")
file_bar.add_command(label="\tOpen\t\t\t\t", command=open_functionality)
file_bar.add("separator")
file_bar.add_command(label="\tSave\t\t\t\t", command=save_functionality)
file_bar.add("separator")
file_bar.add_command(label="\tSave As\t\t\t\t", command=save_as_functionality)
file_bar.add("separator")
file_bar.add_command(label="\tExit\t\t\t\t", command=exit)

themes_bar = Menu(menu_bar, tearoff=0)
themes_bar.add_command(label="\tDark Mode", command=set_dark_mode)
themes_bar.add("separator")
themes_bar.add_command(label="\tLight Mode", command=set_light_mode)

help_bar = Menu(menu_bar, tearoff=0)
help_bar.add_command(label="\tDocuments", command=help_functionality)

menu_bar.add_cascade(label="\tFile\t", menu=file_bar)
menu_bar.add_cascade(label="\tRun\t", command=run_functionality)
menu_bar.add_cascade(label="\tThemes\t", menu=themes_bar)
menu_bar.add_cascade(label="\tHelp\t", menu=help_bar)

window.config(menu=menu_bar)

window.bind('<Control_L><s>', save_functionality)

window.bind('<Control_L><n>', new_page_functionality)
window.bind('<Control_R><n>', new_page_functionality)

window.bind('<Control_L><Shift_L><s>', save_as_functionality)

window.bind('<Alt_L><d>', set_dark_mode)
window.bind('<Alt_R><d>', set_dark_mode)

window.bind('<Alt_L><l>', set_light_mode)
window.bind('<Alt_R><l>', set_light_mode)

window.bind('<Control_L><h>', help_functionality)
window.bind('<Control_R><h>', help_functionality)

window.bind('<Control_L><Shift_L>', run_functionality)

window.bind('<Control_L><o>', open_functionality)
window.bind('<Control_R><o>', open_functionality)

text_editor.focus_set()
window.mainloop()





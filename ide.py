from tkinter import *
from tkinter.filedialog import askopenfilename, asksaveasfilename
from tkinter import font
import webbrowser
import sys

# Import the interpreter file to call the main function
import interpreter

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

# Text editor configuration
text_editor = Text(height=50,
                   width=70,
                   background="#00092C",
                   foreground="#eee",
                   insertbackground="#eee",
                   font=font_family1)
text_editor.insert('1.0', "{ Write your code here }")
text_editor.pack(side=LEFT)

# Console configuration
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
    """
    Creates the new page for writing the source code

    Clears the source code writing screen and set the file path to empty string
    :return: None
    """
    global file_path
    file_path = ""
    text_editor.delete('1.0', END)


def open_functionality():
    """
    Opens the file system and updates the file path to selected file location
    :return: None
    """
    global file_path
    path = askopenfilename(filetypes=[('RVCE Files', '*.rvce')])
    with open(path, 'r') as file:
        code = file.read()
        text_editor.delete('1.0', END)
        text_editor.insert('1.0', code)
        file_path = path


def save_functionality():
    """
    Opens the files system and allows programmer to save the file with .rvce extension
    and hence update the file path if it was empty previously
    :return: None
    """
    global file_path
    if file_path == "":
        path = asksaveasfilename(filetypes=[('RVCE Files', '*.rvce')])
        file_path = path
    else:
        path = file_path
    with open(path, 'w') as file:
        code = text_editor.get('1.0', END)
        file.write(code)


def save_as_functionality():
    """
    Allows the programmer to make a copy of the same file with different name
    :return: None
    """
    global file_path
    path = asksaveasfilename(filetypes=[('RVCE Files', '*.rvce')])
    with open(path, 'w') as file:
        code = text_editor.get('1.0', END)
        file.write(code)
        file_path = path


def set_dark_mode():
    """
    Changes the IDE to dark mode
    :return: None
    """
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


def set_light_mode():
    """
    Changes the IDE to light mode
    :return: None
    """
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


def help_functionality():
    """
    Takes to the documentation poge
    :return:
    """
    webbrowser.open("https://githubsumanth.github.io/Pascal-Documentation/")


def run_functionality():
    """
    Run the source code and produces the output at the console
    :return:
    """
    global file_path
    output_area.delete('1.0', END)
    text = text_editor.get('1.0', END)
    try:
        output = interpreter.main(text)
    except:
        output = sys.exc_info()
    output_area.insert('1.0', output)


# Menubar
menu_bar = Menu(window, background='blue', fg='white')

# Sub-menu Items ---- File
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

# Sub-menu Items ---- Themes
themes_bar = Menu(menu_bar, tearoff=0)
themes_bar.add_command(label="\tDark Mode", command=set_dark_mode)
themes_bar.add("separator")
themes_bar.add_command(label="\tLight Mode", command=set_light_mode)

# Sub-menu Items ---- Help
help_bar = Menu(menu_bar, tearoff=0)
help_bar.add_command(label="\tDocuments", command=help_functionality)

# Menu Item ---- File
menu_bar.add_cascade(label="\tFile\t", menu=file_bar)

# Menu Item ---- Run
menu_bar.add_cascade(label="\tRun\t", command=run_functionality)

# Menu Item ---- Themes
menu_bar.add_cascade(label="\tThemes\t", menu=themes_bar)

# Menu Item ---- Help
menu_bar.add_cascade(label="\tHelp\t", menu=help_bar)

window.config(menu=menu_bar)
text_editor.focus_set()
window.mainloop()





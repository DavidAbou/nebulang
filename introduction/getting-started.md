# 🛠 Getting Started

### Prerequisites

Before you begin, ensure you have the following prerequisites:

* A code editor or IDE of your choice.
* Git installed on your system.
* Rust installed on your system.
* A terminal or command prompt.

### Installation

To start using NebuLang, follow these steps:

#### **Clone the NebuLang Repository**

Open your terminal and run the following command to clone the NebuLang repository from GitHub:

<pre class="language-bash" data-full-width="false"><code class="lang-bash"><strong>git clone https://github.com/DavidAbou/nebulang
</strong></code></pre>

#### Run the install script

```bash
cd nebulang
./install
```

### Running NebuLang programs

Once you have NebuLang installed, you can start writing and running NebuLang programs.

#### Create a NebuLang file

Using your preferred code editor, create a new file with a `.nbl` extension. For example, `hello.nbl`.

#### Write your NebuLang code

{% code title="hello.nbl" %}
```javascript
set message: String = "Hello, World!";
Write(message);
emit 0;
```
{% endcode %}

#### Running your NebuLang file

Open your terminal, navigate to the directory where your NebuLang file is located, and run the NebuLang interpreter with your file as an argument:

```bash
nebulang hello.nbl
```

The output of your program will be displayed in the terminal.

### Congratulations!

You've successfully set up NebuLang and run your first NebuLang program. You can now explore the language further, refer to the documentation for detailed information on NebuLang's features, and start building your own applications.

Happy coding with NebuLang!

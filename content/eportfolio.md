+++
paginate_by = 7
title = "e-Portfolio"
date = "2026-04-16"
insert_anchor_links = "heading"
+++

## Professional Self Assessment
Completing my Computer Science degree has been a valuable experience that helped me grow both technically and professionally. Throughout the program, I developed a stronger ability to approach problems in a structured and analytical way, while also improving how I communicate technical ideas. This ePortfolio reflects my growth in translating requirements into working solutions, making design trade-offs, and building software with clear intent and purpose. It also helped me better understand my own strengths as a developer and how I can contribute in professional environments.

A key area of growth has been my ability to design and evaluate computing solutions using sound engineering and algorithmic thinking. I learned how to break down complex problems into smaller, manageable components and make informed decisions about design trade-offs such as performance, simplicity, and maintainability. I also strengthened my ability to apply industry practices such as modular design, structured testing, and iterative development to ensure solutions are reliable and scalable.

In addition, I improved my ability to work both independently and in collaborative-style workflows by organizing my work in a way that supports readability, maintainability, and shared understanding. I developed stronger communication skills by learning how to present technical solutions clearly for different audiences, whether through documentation, system design, or interface design decisions. I also built a stronger security mindset by consistently considering how systems could be misused, and ensuring that sensitive operations and data are protected through proper validation and access control.

## Course Outcomes

In this ePortfolio I aim to demonstrate the following course outcomes:

- Employ strategies for building collaborative environments that enable diverse audiences to support organizational decision making in the field of computer science
- Design, develop, and deliver professional-quality oral, written, and visual communications that are coherent, technically sound, and appropriately adapted to specific audiences and contexts
- Design and evaluate computing solutions that solve a given problem using algorithmic principles and computer science practices and standards appropriate to its solution, while managing the trade-offs involved in design choices
- Demonstrate an ability to use well-founded and innovative techniques, skills, and tools in computing practices for the purpose of implementing computer solutions that deliver value and accomplish industry-specific goals
- Develop a security mindset that anticipates adversarial exploits in software architecture and designs to expose potential vulnerabilities, mitigate design flaws, and ensure privacy and enhanced security of data and resources

## Context

The artifact is loosely based on the full-stack Android application developed in CS360 that was completed on December 2025 and incorporates all three required enhancements: 

- Software Design and Engineering 
- Algorithms and Data Structures 
- Databases 

I have ensured alignment with all of the course outcomes, and the artifact is intended to demonstrate the skills acquired throughout the course.

{{ note(header="**Code Review**", body="This video provides a walkthrough of the original codebase with a focus on critiquing design decisions, architecture,testing strategy and choice of data structures. ") }}
{{ youtube(id="GD6OGbthwGA", width=662, height=373) }}

## Artifact Description

This project is a **full stack QR code application** that demonstrates my ability to design and build a complete backend system using modern functional programming practices. The application is built using **Scala with Cats Effect** for managing side effects, **http4s** for the HTTP API layer, **Doobie** for database access, and **SQLite** for persistence. It allows users to sign up, log in, generate QR codes from input data, and manage previously generated codes through a REST style API. The project is designed to show how routing, business logic, persistence, and state management can be combined into a cohesive system.

I structured the project in a way that keeps responsibilities clearly separated. The routing layer handles HTTP requests and responses using http4s, the service layer contains the core business logic using Cats Effect, and the database layer manages persistence using Doobie queries against SQLite. I also included utility components for things like QR code generation and JSON encoding. This separation made the code easier to understand, easier to extend, and easier to test. It also helped me make better design decisions because each part of the system has a clear responsibility and limited scope. Session handling is kept separate from core logic to keep authentication clean and predictable.

Testing is a major part of this artifact. I built a full test suite using **MUnit** and **Cats Effect** that covers both service level logic and full API routes. The tests include flows like user registration, login, session handling, QR code generation, history retrieval, and deletion. Instead of only testing small isolated functions, I focused on integration style tests that validate how the system behaves end to end. Each test runs in isolation using a fresh SQLite database instance, which prevents shared state and makes the results reliable and repeatable.

The project also demonstrates how I handle data and system design decisions. I used a relational database schema to store users and QR code records in a structured way, and I used in-memory state for session management to keep things fast and simple. QR code generation involves transforming user input into encoded image data, which requires careful handling of formats and validation. Across the system I also applied security practices such as password hashing, session based authentication, and access control on protected routes to ensure only authorized users can access sensitive operations.


## Enhancement One: Software Design and Engineering 

I chose this artifact because it shows how my approach to software design has improved over time. I originally built this as a simple Android app, but later rebuilt it as a full-stack web application using Scala with http4s, Cats Effect, and Doobie. That transition alone reflects a big shift in how I think about structuring applications and separating concerns.

The strongest part of this project, from a design perspective, is the layered architecture. I split the application into routes, services, and database access instead of putting everything in one place. This made the code much easier to read, test, and extend. Moving logic out of the routes and into a service layer was a big improvement because it made each part of the system responsible for a single role instead of mixing everything together.

I also added proper session management, which forced me to think more carefully about how state flows through the system. That was something I didn’t really handle well in the original version. Refactoring the project helped me understand how different layers should interact without being tightly coupled.

While working on this, one of the biggest challenges was figuring out how to organize everything in a clean way. It’s easy to write code that works, but much harder to structure it so it stays maintainable. I spent a lot of time reworking parts of the project as I learned better practices. At this point, I’ve met the goals I set around improving software design, and I’m mostly focused on polishing things like documentation and fixing a few remaining issues in authentication.

## Enhancement Two: Algorithms and Data Structures

I picked this artifact because it highlights how I applied core logic and data handling in a real application, not just in isolated problems. Even though it’s a full-stack project, there are several places where algorithms and data structures play an important role.

One example is the session management system, where I had to decide how to store and retrieve session data efficiently. I used appropriate data structures to track sessions and ensure quick lookups while keeping things simple and reliable. Another area is how inventory data is processed and returned through the API, where I had to think about transformations, filtering, and structuring responses in a clean way.

The QR code generation feature also ties into this, since it required encoding data in a consistent format that could be generated and read reliably. While it’s not a complex algorithm-heavy feature, it still required careful handling of input and output data.

As I improved the project, I focused on making the logic clearer and more efficient. Early on, some of my code was more repetitive and less organized, but I refactored it to reduce redundancy and make the data flow easier to follow. This helped me better understand how to structure logic in a way that’s both readable and efficient.

One challenge I ran into was balancing simplicity with correctness. It’s easy to overcomplicate things when thinking about edge cases, but I learned to focus on clean, practical solutions. Overall, this project helped me connect abstract concepts like data structures and algorithms to real-world application development.

## Enhancement Three: Databases

I included this artifact because it demonstrates my ability to work directly with a relational database and integrate it into an application. The project uses SQLite with Doobie, which gave me hands-on experience writing SQL and handling database interactions in a functional programming context.

The most important part of this project, from a database perspective, is the way I structured and accessed data. I wrote SQL queries using Doobie to handle operations like user authentication, inventory management, and session storage. This required me to think carefully about how data is stored, retrieved, and mapped into application types.

One of the biggest improvements I made was cleaning up the database layer. In the earlier version, database logic wasn’t very well organized, but I refactored it so that queries were more structured and easier to maintain. I also improved how data is converted between database rows and Scala types, which helped reduce errors and made the code more predictable.

I also learned more about transactions and consistency, especially when dealing with things like user login and session creation. Making sure the database state stays correct across different operations was something I had to think through more carefully than I expected.

The main challenge here was getting comfortable with how Doobie works and making sure my queries were both correct and type-safe. Debugging database issues can be tricky, especially when the problem is in how data is mapped rather than the query itself. But working through that helped me build a much stronger understanding of database integration overall.

## Final Work

The final implementation of the project is hosted on GitHub:  
- **Final Application:** <https://github.com/ajafri2001/QrCodeApp>

For comparison and development history, the original project artifact is available here:  
- **Original Artifact:** <https://github.com/ajafri2001/CS-360>

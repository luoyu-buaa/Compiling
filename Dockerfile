FROM openjdk:8
COPY ./* /app/
WORKDIR /app/
RUN javac -d ./output ./MyTest.java
WORKDIR /app/output
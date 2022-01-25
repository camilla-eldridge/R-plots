library(DiagrammeR)

DiagrammeR("graph LR;
           A[Are you procrastinating?]--> B{Yes};
           A-->C{No};
           B-->D((Stop it));
           C-->E>Yes you are];
           E-->F((Stop it)); 
           style A fill:#E5E25F;
           style B fill:#3498db; 
           style C fill:#a569bd;
           style D fill:#C70039;
           style E fill:#f39c12;
           style F fill:#C70039;")

#colour codes ; https://htmlcolorcodes.com/ 


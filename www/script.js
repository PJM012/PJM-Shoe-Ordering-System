/* =========================================================
  GLOBAL NUMBER FORMATTER (SAFE + REUSABLE)
========================================================= */
function formatText(text) {
  if (!text || typeof text !== 'string') return text;
  
  const cleanText = text.replace(/,/g, '');
  
  // Currency (₱)
  if (cleanText.includes('₱')) {
    const amount = cleanText.replace(/[^0-9.]/g, '');
    if (!isNaN(amount)) {
      return '₱' + Number(amount).toLocaleString('en-US', {
        minimumFractionDigits: 2,
        maximumFractionDigits: 2
      });
    }
  }
  
  // Stock / Quantity (NO decimals)
  if (text.includes('Stock')) {
    const match = cleanText.match(/\d+/);
    if (match) {
      return text.replace(match[0], Number(match[0]).toLocaleString());
    }
  }
  
  // Plain number
  if (/^\d+(\.\d+)?$/.test(cleanText)) {
    return Number(cleanText).toLocaleString();
  }
  
  // Number inside text
  return text.replace(/\d+(\.\d+)?/g, m =>
    Number(m).toLocaleString('en-US', {
      minimumFractionDigits: text.includes('Price') ? 2 : 0,
      maximumFractionDigits: text.includes('Price') ? 2 : 0
    })
  );
}

/* =========================================================
  SIMPLE PASSWORD TOGGLE FUNCTION
========================================================= */
function togglePasswordVisibility(button) {
  const wrapper = button.closest('.password-input-wrapper');
  const input = wrapper.querySelector('input');
  const icon = button.querySelector('i');
  
  if (input.type === 'password') {
    input.type = 'text';
    icon.classList.remove('fa-eye');
    icon.classList.add('fa-eye-slash');
    button.setAttribute('aria-label', 'Hide password');
  } else {
    input.type = 'password';
    icon.classList.remove('fa-eye-slash');
    icon.classList.add('fa-eye');
    button.setAttribute('aria-label', 'Show password');
  }
  input.focus();
}

/* =========================================================
  PRODUCT NAME LINE BREAKING FOR TABLES
========================================================= */
function fixProductNameLineBreaks() {
  // Function to break long product names
  function breakProductName(name, maxLength = 15) {
    if (name.length <= maxLength) return name;
    
    const words = name.split(' ');
    if (words.length === 1) {
      // Single long word - break by character
      return name.match(new RegExp(`.{1,${maxLength}}`, 'g')).join('<br>');
    } else {
      // Multiple words - try to fit optimally
      let lines = [];
      let currentLine = '';
      
      for (let word of words) {
        if (currentLine.length + word.length + 1 <= maxLength) {
          currentLine = currentLine ? currentLine + ' ' + word : word;
        } else {
          if (currentLine) lines.push(currentLine);
          currentLine = word.length > maxLength ? 
            word.match(new RegExp(`.{1,${maxLength}}`, 'g')).join('<br>') : 
            word;
        }
      }
      
      if (currentLine) lines.push(currentLine);
      return lines.join('<br>');
    }
  }
  
  // Apply to cart table
  document.querySelectorAll('#cart_table_display td:first-child').forEach(cell => {
    const originalText = cell.textContent.trim();
    if (originalText.length > 15) {
      cell.innerHTML = breakProductName(originalText, 15);
      cell.style.lineHeight = '1.3';
      cell.style.padding = '10px 5px';
      cell.style.minHeight = '60px';
      cell.style.display = 'flex';
      cell.style.alignItems = 'center';
      cell.style.justifyContent = 'center';
      cell.style.flexDirection = 'column';
      cell.classList.add('cart-product-name', 'stacked');
    }
  });
  
  // Apply to order history tables
  document.querySelectorAll('.order-history-modal td:first-child').forEach(cell => {
    const originalText = cell.textContent.trim();
    if (originalText.length > 15) {
      cell.innerHTML = breakProductName(originalText, 15);
      cell.style.lineHeight = '1.3';
      cell.style.padding = '10px 5px';
    }
  });
  
  // Apply to staff order items table
  document.querySelectorAll('#order_items_display td:first-child').forEach(cell => {
    const originalText = cell.textContent.trim();
    if (originalText.length > 15) {
      cell.innerHTML = breakProductName(originalText, 15);
      cell.style.lineHeight = '1.3';
      cell.style.padding = '10px 5px';
    }
  });
}

/* =========================================================
  CART TABLE HEIGHT AND SCROLLING FIX - NO SCROLL, FIT TO MODAL
========================================================= */
function fixCartTableScrolling() {
  const cartWrapper = document.querySelector('#cart_table_display_wrapper');
  const cartTable = document.querySelector('#cart_table_display');
  
  if (!cartWrapper || !cartTable) return;
  
  // Reset styles for no-scroll, fit to modal
  cartWrapper.style.overflowX = 'auto';
  cartWrapper.style.overflowY = 'auto';
  cartWrapper.style.maxHeight = '250px'; // Reduced height for better fit
  cartWrapper.style.position = 'relative';
  cartWrapper.style.marginBottom = '10px';
  
  // Calculate optimal height based on content
  const rowCount = cartTable.querySelectorAll('tbody tr').length;
  const rowHeight = 45; // Reduced row height
  const headerHeight = 45; // Reduced header height
  const padding = 10;
  
  let calculatedHeight = (rowCount * rowHeight) + headerHeight + padding;
  
  // Limit height to prevent overflow
  calculatedHeight = Math.min(calculatedHeight, 250);
  
  cartWrapper.style.maxHeight = calculatedHeight + 'px';
  cartWrapper.style.height = calculatedHeight + 'px';
  
  // Optimize table layout
  cartTable.style.width = '100%';
  cartTable.style.tableLayout = 'auto';
  
  // Set specific column widths for better fit
  const columns = cartTable.querySelectorAll('th');
  if (columns.length >= 6) {
    columns[0].style.minWidth = '120px';
    columns[0].style.maxWidth = '150px';
    columns[1].style.minWidth = '70px';
    columns[2].style.minWidth = '70px';
    columns[3].style.minWidth = '60px';
    columns[4].style.minWidth = '90px';
    columns[5].style.minWidth = '90px';
  }
  
  // Ensure header stays visible
  const thead = cartTable.querySelector('thead');
  if (thead) {
    thead.style.position = 'sticky';
    thead.style.top = '0';
    thead.style.zIndex = '10';
    thead.style.backgroundColor = 'white';
    thead.style.boxShadow = '0 2px 4px rgba(0,0,0,0.1)';
  }
  
  // Adjust DataTables if present
  if (typeof $ !== 'undefined' && $(cartTable).DataTable) {
    const dataTable = $(cartTable).DataTable();
    if (dataTable) {
      dataTable.columns.adjust();
      dataTable.responsive.recalc();
    }
  }
}

/* =========================================================
  APPLY STATUS BADGE STYLES (UPDATED)
========================================================= */
function applyStatusBadgeStyles() {
  // Find all status cells and apply badge styles
  document.querySelectorAll('td, span, p, div').forEach(el => {
    const text = el.textContent.trim();
    const lowerText = text.toLowerCase();
    
    // Skip elements that already have badge classes
    if (el.innerHTML.includes('status-') && el.innerHTML.includes('-badge')) {
      return;
    }
    
    if (lowerText === 'pending' || text === 'Pending') {
      el.innerHTML = `<span class="status-pending-badge">Pending</span>`;
      el.classList.add('status-cell');
    } else if (lowerText === 'processing' || text === 'Processing') {
      el.innerHTML = `<span class="status-processing-badge">Processing</span>`;
      el.classList.add('status-cell');
    } else if (lowerText === 'to ship' || text === 'To Ship') {
      el.innerHTML = `<span class="status-to-ship-badge">To Ship</span>`;
      el.classList.add('status-cell');
    } else if (lowerText === 'shipped' || text === 'Shipped') {
      el.innerHTML = `<span class="status-shipped-badge">Shipped</span>`;
      el.classList.add('status-cell');
    } else if (lowerText === 'completed' || text === 'Completed') {
      el.innerHTML = `<span class="status-completed-badge">Completed</span>`;
      el.classList.add('status-cell');
    } else if (lowerText === 'cancelled' || text === 'Cancelled') {
      el.innerHTML = `<span class="status-cancelled-badge">Cancelled</span>`;
      el.classList.add('status-cell');
    }
    
    // Also check for text that contains status labels
    if (text.includes('Status:') || text.includes('STATUS:')) {
      const statusMatch = text.match(/status:\s*([a-zA-Z\s]+)/i);
      if (statusMatch) {
        const status = statusMatch[1].trim();
        const statusLower = status.toLowerCase();
        
        let badgeClass = '';
        if (statusLower === 'pending') badgeClass = 'status-pending-badge';
        else if (statusLower === 'processing') badgeClass = 'status-processing-badge';
        else if (statusLower === 'to ship') badgeClass = 'status-to-ship-badge';
        else if (statusLower === 'shipped') badgeClass = 'status-shipped-badge';
        else if (statusLower === 'completed') badgeClass = 'status-completed-badge';
        else if (statusLower === 'cancelled') badgeClass = 'status-cancelled-badge';
        
        if (badgeClass) {
          el.innerHTML = el.innerHTML.replace(
            status, 
            `<span class="${badgeClass}">${status}</span>`
          );
        }
      }
    }
  });
  
  // Special handling for order success modal
  document.querySelectorAll('.order-summary-card, .modal-body').forEach(container => {
    const html = container.innerHTML;
    if (html.includes('Status:') || html.includes('STATUS:')) {
      container.innerHTML = html.replace(
        /Status:\s*(Pending|Processing|To Ship|Shipped|Completed|Cancelled)/gi,
        'Status: <span class="status-$1-badge">$1</span>'
      );
    }
  });
}

/* =========================================================
  DATE FORMATTING FOR ORDER HISTORY TABLES
========================================================= */
function fixDateFormatting() {
  // Format dates in all DataTables
  document.querySelectorAll('.dataTable td').forEach(cell => {
    const text = cell.textContent.trim();
    
    // Match date patterns like "2024-01-01 12:00:00" or "2024-01-01"
    if (/^\d{4}-\d{2}-\d{2}/.test(text)) {
      try {
        const date = new Date(text);
        if (!isNaN(date)) {
          // Format consistently across all tables
          const formattedDate = date.toLocaleString('en-US', {
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
            hour: '2-digit',
            minute: '2-digit',
            second: '2-digit',
            hour12: true
          });
          
          // Clean up the format to be consistent
          const cleanedDate = formattedDate.replace(',', '');
          cell.textContent = cleanedDate;
          cell.classList.add('date-cell');
        }
      } catch (e) {
        console.log('Date formatting error:', e);
      }
    }
  });
}

/* =========================================================
  CSS PERSISTENCE SYSTEM - ENHANCED
========================================================= */
function initializeCSSPersistence() {
  console.log('Initializing CSS persistence system...');
  
  // Add CSS loaded class to body
  document.body.classList.add('css-loaded');
  
  // Force reflow to ensure CSS is applied
  document.body.style.opacity = '1';
  document.body.style.visibility = 'visible';
  
  // Store CSS state
  localStorage.setItem('cssLoaded', 'true');
  
  // Apply styles to login container
  const loginContainer = document.getElementById('login_container');
  if (loginContainer) {
    loginContainer.style.opacity = '1';
    loginContainer.style.visibility = 'visible';
    loginContainer.style.display = 'flex';
  }
  
  // Initialize password toggles
  setTimeout(initPasswordToggles, 100);
}

/* =========================================================
  FORCE CSS RELOAD FOR LOGOUT ISSUE
========================================================= */
function forceCSSReload() {
  console.log('Forcing CSS reload...');
  
  // Remove CSS classes
  document.body.classList.remove('css-loaded');
  
  // Force reflow
  void document.body.offsetHeight;
  
  // Re-add classes
  setTimeout(() => {
    document.body.classList.add('css-loaded');
    
    // Reinitialize login container
    const loginContainer = document.getElementById('login_container');
    if (loginContainer) {
      loginContainer.style.opacity = '1';
      loginContainer.style.visibility = 'visible';
      loginContainer.style.display = 'flex';
    }
    
    // Reinitialize all components
    setTimeout(init, 200);
  }, 10);
}

/* =========================================================
  ENHANCED PASSWORD TOGGLE SYSTEM
========================================================= */
function initPasswordToggles() {
  console.log('Initializing password toggles...');
  
  // Find all password inputs including dynamically created ones
  const passwordInputs = document.querySelectorAll('input[type="password"]');
  
  passwordInputs.forEach(input => {
    // Skip if already has toggle
    if (input.hasAttribute('data-toggle-added')) return;
    
    // Create wrapper if it doesn't exist
    let wrapper = input.parentElement;
    if (!wrapper || !wrapper.classList.contains('password-input-wrapper')) {
      wrapper = document.createElement('div');
      wrapper.className = 'password-input-wrapper';
      wrapper.style.position = 'relative';
      wrapper.style.width = '100%';
      wrapper.style.display = 'block';
      wrapper.style.marginBottom = '15px';
      
      // Wrap the input
      input.parentNode.insertBefore(wrapper, input);
      wrapper.appendChild(input);
    }
    
    // Remove existing toggle button if any
    const existingToggle = wrapper.querySelector('.password-toggle-btn');
    if (existingToggle) {
      existingToggle.remove();
    }
    
    // Create toggle button
    const toggleBtn = document.createElement('button');
    toggleBtn.type = 'button';
    toggleBtn.className = 'password-toggle-btn';
    toggleBtn.innerHTML = '<i class="fas fa-eye"></i>';
    toggleBtn.setAttribute('aria-label', 'Show password');
    toggleBtn.setAttribute('title', 'Show password');
    
    // Add click handler
    toggleBtn.addEventListener('click', function() {
      const isPassword = input.type === 'password';
      input.type = isPassword ? 'text' : 'password';
      this.innerHTML = isPassword ? '<i class="fas fa-eye-slash"></i>' : '<i class="fas fa-eye"></i>';
      this.setAttribute('aria-label', isPassword ? 'Hide password' : 'Show password');
      this.setAttribute('title', isPassword ? 'Hide password' : 'Show password');
      input.focus();
    });
    
    // Add button to wrapper
    wrapper.appendChild(toggleBtn);
    
    // Adjust input padding
    input.style.paddingRight = '45px';
    input.style.width = '100%';
    input.style.boxSizing = 'border-box';
    input.style.height = '42px';
    
    // Mark as processed
    input.setAttribute('data-toggle-added', 'true');
  });
  
  // Also handle text inputs that should be password fields
  const possiblePasswordInputs = document.querySelectorAll('input[name*="pass"], input[name*="Pass"]');
  possiblePasswordInputs.forEach(input => {
    if (input.type === 'text' && !input.hasAttribute('data-toggle-added')) {
      input.type = 'password';
      // Re-run toggles for these
      setTimeout(initPasswordToggles, 100);
    }
  });
}

/* =========================================================
   NUMBER FORMATTING
========================================================= */
function formatNumbersWithCommas() {
  document.querySelectorAll(
    '.dataTable td, .dataTable th, p, span, h4, h5, div'
  ).forEach(el => {
    const text = el.textContent.trim();
    if (!text) return;

    if (
      text.includes('₱') ||
      text.includes('Price') ||
      text.includes('Total') ||
      text.includes('Amount') ||
      text.includes('Stock')
    ) {
      const formatted = formatText(text);
      if (formatted !== text) {
        el.textContent = formatted;
        el.classList.add('comma-number');
      }
    }
  });
}

/* =========================================================
   MODAL CENTERING FIX
========================================================= */
function fixAllModalCentering() {
  document.querySelectorAll('.modal.show').forEach(modal => {
    modal.style.display = 'flex';
    modal.style.alignItems = 'center';
    modal.style.justifyContent = 'center';
    modal.querySelector('.modal-dialog').style.maxHeight = '90vh';
  });
}

/* =========================================================
   ADD TO CART MODAL LAYOUT FIX - CANCEL BUTTON CENTERED
========================================================= */
function fixAddToCartModalLayout() {
  const modal = document.querySelector('.modal.show');
  if (!modal) return;
  
  const footer = modal.querySelector('.modal-footer');
  if (footer) {
    // Check if this is an add to cart modal
    const modalTitle = modal.querySelector('.modal-title');
    if (modalTitle && (modalTitle.textContent.includes('Add') || 
                       modalTitle.textContent.includes('Cart'))) {
      // Ensure proper flex layout for add to cart modal
      footer.style.display = 'flex';
      footer.style.flexDirection = 'column';
      footer.style.gap = '10px';
      footer.style.padding = '15px 0 0 0';
      footer.style.width = '100%';
      
      // Style all buttons in the modal footer
      const buttons = footer.querySelectorAll('button');
      buttons.forEach((btn, index) => {
        btn.style.width = '100%';
        btn.style.margin = '0';
        btn.style.order = index + 1;
        
        // Add cancel class to appropriate buttons
        if (btn.textContent.includes('Cancel') || 
            btn.classList.contains('btn-default')) {
          btn.classList.add('btn-cancel');
          btn.classList.remove('btn-default');
        }
        
        // Add continue class to appropriate buttons
        if (btn.textContent.includes('Continue Shopping')) {
          btn.classList.add('btn-continue');
        }
      });
    }
  }
}

/* =========================================================
   CART TABLE FIX - NO SCROLL, FIT TO MODAL
========================================================= */
function fixCartTable() {
  const cartWrapper = document.querySelector('#cart_table_display_wrapper');
  const cartTable = document.querySelector('#cart_table_display');
  if (!cartWrapper || !cartTable) return;

  // Calculate optimal height based on content
  const rowCount = cartTable.querySelectorAll('tbody tr').length;
  const rowHeight = 45;
  const headerHeight = 45;
  const padding = 10;

  let calculatedHeight = (rowCount * rowHeight) + headerHeight + padding;
  calculatedHeight = Math.min(calculatedHeight, 250); // Max 250px

  cartWrapper.style.maxHeight = calculatedHeight + 'px';
  cartWrapper.style.height = calculatedHeight + 'px';
  cartTable.style.width = '100%';

  setTimeout(formatNumbersWithCommas, 100);
}

/* =========================================================
   TOAST SYSTEM
========================================================= */
window.showToast = function (message, type = 'success', duration = 3000) {
  const container = document.querySelector('.toast-container') || (() => {
    const c = document.createElement('div');
    c.className = 'toast-container';
    document.body.appendChild(c);
    return c;
  })();
  
  const icons = {
    success: 'check-circle',
    error: 'times-circle',
    warning: 'exclamation-triangle',
    info: 'info-circle'
  };
  
  const toast = document.createElement('div');
  toast.className = `toast toast-${type}`;
  toast.innerHTML = `
    <div class="toast-icon"><i class="fa fa-${icons[type]}"></i></div>
    <div class="toast-message">${message}</div>
    <button class="toast-close">&times;</button>
  `;
  
  container.appendChild(toast);
  
  toast.querySelector('.toast-close').onclick = () => {
    toast.remove();
  };
  
  setTimeout(() => {
    if (toast.parentNode) {
      toast.remove();
    }
  }, duration);
};

/* =========================================================
  ENHANCED INITIALIZATION FOR SHINYAPPS.IO
========================================================= */
function initForShinyapps() {
  console.log('Initializing for shinyapps.io deployment...');
  
  // Force CSS persistence
  document.body.classList.add('css-loaded');
  document.body.style.opacity = '1';
  document.body.style.visibility = 'visible';
  
  // Initialize all components with delays
  setTimeout(initPasswordToggles, 100);
  setTimeout(formatNumbersWithCommas, 200);
  setTimeout(applyStatusBadgeStyles, 300);
  setTimeout(fixAllModalCentering, 400);
  
  // Special handling for shinyapps.io
  if (window.location.hostname.includes('shinyapps.io')) {
    console.log('Running on shinyapps.io - applying special fixes');
    
    // More aggressive CSS persistence
    setInterval(function() {
      if (!document.body.classList.contains('css-loaded')) {
        document.body.classList.add('css-loaded');
      }
    }, 1000);
  }
}

/* =========================================================
  ENHANCED INITIALIZATION FUNCTION
========================================================= */
function init() {
  console.log('Initializing shoe ordering system...');
  
  // Initialize all components
  initializeCSSPersistence();
  initPasswordToggles();
  formatNumbersWithCommas();
  fixDateFormatting();
  fixCartTable();
  fixAllModalCentering();
  fixProductNameLineBreaks();
  fixCartTableScrolling();
  fixAddToCartModalLayout();
  
  // Apply status styles with delay to ensure DOM is ready
  setTimeout(() => {
    applyStatusBadgeStyles();
  }, 300);
  
  // Monitor for new password fields
  const observer = new MutationObserver((mutations) => {
    mutations.forEach((mutation) => {
      if (mutation.addedNodes.length) {
        mutation.addedNodes.forEach((node) => {
          if (node.nodeType === 1) {
            // Check for password inputs
            if (node.querySelector && 
                (node.querySelector('input[type="password"]') || 
                 node.querySelector('input[name*="pass"]'))) {
              setTimeout(initPasswordToggles, 100);
            }
            
            // Check for status displays
            if (node.textContent && 
                (node.textContent.toLowerCase().includes('status') ||
                 node.querySelector && 
                 (node.querySelector('[class*="status"]') ||
                  node.querySelector('td')))) {
              setTimeout(applyStatusBadgeStyles, 150);
            }
            
            // Check for cart table
            if (node.querySelector && node.querySelector('#cart_table_display')) {
              setTimeout(fixCartTableScrolling, 200);
              setTimeout(fixProductNameLineBreaks, 200);
            }
            
            // Check for modals
            if (node.classList && node.classList.contains('modal')) {
              setTimeout(fixAddToCartModalLayout, 100);
            }
          }
        });
      }
    });
  });
  
  observer.observe(document.body, {
    childList: true,
    subtree: true
  });
}

/* =========================================================
  SHINY CUSTOM MESSAGE HANDLERS
========================================================= */
if (window.Shiny) {
  Shiny.addCustomMessageHandler('showToast', function(message) {
    window.showToast(message.text, message.type, message.duration || 3000);
  });
  
  Shiny.addCustomMessageHandler('formatNumbers', function() {
    setTimeout(formatNumbersWithCommas, 100);
  });
  
  Shiny.addCustomMessageHandler('refreshUI', function() {
    setTimeout(init, 100);
  });
  
  Shiny.addCustomMessageHandler('refreshCSS', function(message) {
    setTimeout(initializeCSSPersistence, 50);
  });
  
  Shiny.addCustomMessageHandler('forceCSSReload', function(message) {
    forceCSSReload();
  });
  
  Shiny.addCustomMessageHandler('applyStatusStyles', function() {
    setTimeout(applyStatusBadgeStyles, 100);
  });
  
  Shiny.addCustomMessageHandler('fixModalLayout', function() {
    setTimeout(fixAddToCartModalLayout, 100);
  });
}

/* =========================================================
  EVENT LISTENERS
========================================================= */
document.addEventListener('DOMContentLoaded', function() {
  setTimeout(initForShinyapps, 100);
  // Initial initialization
  setTimeout(init, 500);
  
  // Monitor for modal openings
  $(document).on('shown.bs.modal', function(e) {
    const modal = e.target;
    
    setTimeout(() => {
      fixAllModalCentering();
      formatNumbersWithCommas();
      fixDateFormatting();
      applyStatusBadgeStyles();
      fixAddToCartModalLayout();
      
      // Check if this is a cart modal
      if (modal.querySelector('#cart_table_display')) {
        fixCartTableScrolling();
        fixProductNameLineBreaks();
      }
      
      // Check if this is an order history modal
      if (modal.querySelector('.order-history-modal')) {
        fixProductNameLineBreaks();
      }
    }, 100);
  });
  
  $(document).on('hidden.bs.modal', function() {
    // Reapply styles after modal closes
    setTimeout(applyStatusBadgeStyles, 50);
  });
});

/* =========================================================
  MUTATION OBSERVER FOR DYNAMIC CONTENT
========================================================= */
const observer = new MutationObserver((mutations) => {
  let shouldUpdate = false;
  let cartUpdated = false;
  let statusUpdated = false;
  let modalUpdated = false;
  let loginUpdated = false;
  
  mutations.forEach(mutation => {
    if (mutation.type === 'childList') {
      mutation.addedNodes.forEach(node => {
        if (node.nodeType === 1) {
          // Check for login container updates
          if (node.id === 'login_container' || 
              (node.classList && node.classList.contains('login-box'))) {
            loginUpdated = true;
          }
          
          // Check for tables
          if (node.classList && 
              (node.classList.contains('dataTable') || 
               node.querySelector('.dataTable') ||
               node.id === 'customer_completed' ||
               node.id === 'staff_orders_table' ||
               node.id === 'cart_table_display')) {
            shouldUpdate = true;
          }
          
          // Check for cart modal
          if (node.classList && node.classList.contains('modal-content')) {
            if (node.querySelector('#cart_table_display')) {
              cartUpdated = true;
            }
            modalUpdated = true;
          }
          
          // Check for status updates
          if (node.textContent && 
              (node.textContent.toLowerCase().includes('status') ||
               node.querySelector && node.querySelector('[class*="status"]'))) {
            statusUpdated = true;
          }
        }
      });
    }
  });
  
  if (loginUpdated) {
    setTimeout(() => {
      forceCSSReload();
    }, 200);
  }
  
  if (shouldUpdate) {
    setTimeout(() => {
      initPasswordToggles();
      formatNumbersWithCommas();
      fixDateFormatting();
      fixProductNameLineBreaks();
    }, 100);
  }
  
  if (cartUpdated) {
    setTimeout(() => {
      fixCartTableScrolling();
      fixProductNameLineBreaks();
    }, 150);
  }
  
  if (statusUpdated) {
    setTimeout(applyStatusBadgeStyles, 100);
  }
  
  if (modalUpdated) {
    setTimeout(fixAddToCartModalLayout, 100);
  }
});

// Start observing
observer.observe(document.body, { 
  childList: true, 
  subtree: true,
  attributes: true,
  attributeFilter: ['class', 'id', 'style']
});

/* =========================================================
  NAVIGATION MONITOR
========================================================= */
let lastHref = window.location.href;
setInterval(() => {
  if (window.location.href !== lastHref) {
    lastHref = window.location.href;
    // Reinitialize on navigation
    setTimeout(init, 300);
  }
}, 100);

/* =========================================================
  GLOBAL EXPORTS
========================================================= */
window.formatNumbersWithCommas = formatNumbersWithCommas;
window.fixCartTable = fixCartTable;
window.fixAllModalCentering = fixAllModalCentering;
window.fixAddToCartModalLayout = fixAddToCartModalLayout;
window.fixCartTableScrolling = fixCartTableScrolling;
window.fixDateFormatting = fixDateFormatting;
window.initPasswordToggles = initPasswordToggles;
window.initializeCSSPersistence = initializeCSSPersistence;
window.forceCSSReload = forceCSSReload;
window.fixProductNameLineBreaks = fixProductNameLineBreaks;
window.applyStatusBadgeStyles = applyStatusBadgeStyles;
window.togglePasswordVisibility = togglePasswordVisibility;

// Initialize on load
setTimeout(init, 1000);
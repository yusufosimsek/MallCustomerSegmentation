import React, { useState } from 'react';
import './App.css';

// Import images from assets folder
import clusterChart from './assets/Rplot01.png'; 
import distChart from './assets/Rplot02.png';

// Content dictionary for multi-language support
const translations = {
  en: {
    title: "Mall Customer Segmentation Analysis",
    subtitle: "Comparative Report: V1 (Distribution) vs V2 (Inferential Statistics)",
    stats: {
      total: "Total Customers",
      age: "Avg. Age",
      income: "Avg. Annual Income",
      score: "Avg. Spending Score"
    },
    tabs: {
      overview: "Overview & Goal",
      analysis: "Data Analysis (V1 vs V2)",
      strategy: "Strategy & Recommendations"
    },
    overview: {
      goalTitle: "Project Goal",
      goalDesc: "This project aims to develop data-driven marketing strategies using inferential statistics (V2) rather than relying solely on descriptive methods (V1). The primary objective is to validate the true relationship between income and spending behavior.",
      mythTitle: "The Common Misconception (V1)",
      mythDesc: "In the initial analysis (V1), it was assumed that high-income customers are automatically 'VIPs' (High Spenders). However, statistical tests did not validate this assumption.",
      realityTitle: "The Reality (V2)",
      realityDesc: "Inferential analyses revealed that spending behavior is more strongly correlated with age groups rather than income. The younger demographic shows the highest engagement with the brand, regardless of their income levels."
    },
    analysis: {
      compTitle: "Methodology Comparison",
      tableHead: ["Feature", "V1: Distribution Based", "V2: Inferential Statistics"],
      rows: [
        ["Approach", "K-Means & Descriptive", "Hypothesis Testing (MANOVA)"],
        ["VIP Definition", "High Income = High Spending", "Statistically Insignificant"],
        ["Focus Variable", "Annual Income", "Age Groups"]
      ],
      v1Title: "V1: K-Means Clustering Result",
      v1Desc: "The V1 analysis created 5 clusters based on income and spending scores. However, V2 proved that these clusters (especially the 'VIP' group) were not statistically significant.",
      v1Caption: "Figure 1: Customer Segments by Income and Spending Score (Rplot01)",
      v2Title: "V2: Data Distribution & EDA Findings",
      v2Intro: "Examination of variable distributions (Histogram and Boxplot):",
      v2List: [
        "Age: Left-skewed distribution, high concentration of young people.",
        "Income: Near-normal distribution but with outliers.",
        "Spending: Centered distribution, but the median is closer to the younger group."
      ],
      v2Highlight: "Charts visually confirm the numerical dominance and spending potential of the 'Young' demographic.",
      v2Caption: "Figure 2: Distribution of Age, Income, and Spending (Rplot02)",
      genderTitle: "Gender Analysis",
      genderDesc: "T-Tests and MANOVA results showed no statistically significant difference between gender and spending score or annual income (p > 0.05).",
      genderResult: "Conclusion: Developing gender-based marketing strategies is not supported by the data.",
      trendTitle: "Spending Trend by Segment",
      trendLabels: ["Youth (~18-35)", "Middle Age (36-55)", "Seniors (55+)"],
      trendLevels: ["High", "Medium", "Low"],
      trendNote: "* According to the report, the highest spending score is in the young group (Group 1), while the highest income is in the middle-aged group (Group 2)."
    },
    strategy: {
      mainTitle: "Final Business Strategy",
      mainDesc: "Based on the data, the brand should shift from 'income-growth assumption' strategies to 'engagement-focused' strategies.",
      target1Title: "1. Primary Target: The Youth",
      target1List: [
        "Strategy: Income-agnostic, gender-neutral approach.",
        "Action: Campaigns themed around 'Delighting the Youth'.",
        "Reason: Highest spending score and brand loyalty potential."
      ],
      target2Title: "2. Secondary Target: Middle Age (36-40)",
      target2List: [
        "Strategy: Gifting and special occasion focus.",
        "Action: Directing the high purchasing power of this group towards spending for others.",
        "Reason: They have the highest income but lower spending scores; they need triggers."
      ],
      avoidTitle: "3. What to Avoid",
      avoidList: [
        "Mistake: Treating customers as VIPs solely based on High Income.",
        "Risk: V2 analysis proved that high income does not guarantee high spending. This strategy may lead to resource waste."
      ]
    },
    footer: "Source: Mall Customer Segmentation Report - Final.pdf & Analysis Scripts"
  },
  tr: {
    title: "AVM Müşteri Segmentasyon Analizi",
    subtitle: "V1 (Dağılım) ve V2 (Çıkarımsal İstatistik) Karşılaştırmalı Raporu",
    stats: {
      total: "Toplam Müşteri",
      age: "Ortalama Yaş",
      income: "Ort. Yıllık Gelir",
      score: "Ort. Harcama Skoru"
    },
    tabs: {
      overview: "Özet & Amaç",
      analysis: "Veri Analizi (V1 vs V2)",
      strategy: "Strateji ve Öneriler"
    },
    overview: {
      goalTitle: "Proje Amacı",
      goalDesc: "Bu proje, müşteri segmentasyonunda sadece betimsel yöntemlere (V1) güvenmek yerine, çıkarımsal istatistik (V2) kullanarak veriye dayalı pazarlama stratejileri geliştirmeyi amaçlar. Temel hedef, gelir ve harcama arasındaki gerçek ilişkiyi doğrulamaktır.",
      mythTitle: "Temel Yanılgı (V1)",
      mythDesc: "İlk analizde (V1), yüksek gelirli müşterilerin otomatik olarak 'VIP' (Yüksek Harcama Yapan) olduğu varsayılmıştır. Ancak istatistiksel testler bu varsayımı doğrulamamıştır.",
      realityTitle: "Gerçek Durum (V2)",
      realityDesc: "Çıkarımsal analizler, harcama davranışının gelirden ziyade yaş grupları ile daha güçlü bir ilişkisi olduğunu ortaya koymuştur. Genç kitle, gelirlerinden bağımsız olarak marka ile en yüksek etkileşimi göstermektedir."
    },
    analysis: {
      compTitle: "Metodoloji Karşılaştırması",
      tableHead: ["Özellik", "V1: Dağılım Bazlı", "V2: Çıkarımsal İstatistik"],
      rows: [
        ["Yaklaşım", "K-Means & Betimsel", "Hipotez Testleri (MANOVA)"],
        ["VIP Tanımı", "Yüksek Gelir = Yüksek Harcama", "İstatistiksel olarak anlamsız"],
        ["Odak Değişken", "Yıllık Gelir", "Yaş Grupları"]
      ],
      v1Title: "V1: K-Means Kümeleme Sonucu",
      v1Desc: "İlk analizde (V1), gelir ve harcama skoruna göre 5 küme oluşturulmuştur. Ancak V2 analizi bu kümelerin (özellikle 'VIP' grubunun) istatistiksel olarak anlamlı olmadığını kanıtlamıştır.",
      v1Caption: "Şekil 1: Gelir ve Harcama Skoruna Göre Müşteri Kümeleri (Rplot01)",
      v2Title: "V2: Veri Dağılımı ve EDA Bulguları",
      v2Intro: "Değişkenlerin dağılımı incelendiğinde (Histogram ve Boxplot):",
      v2List: [
        "Yaş: Sola çarpık bir dağılım var, genç nüfus yoğunlukta.",
        "Gelir: Normal dağılıma yakın ancak aykırı değerler mevcut.",
        "Harcama: Ortada toplanmış görünse de medyan değer gençlere daha yakın."
      ],
      v2Highlight: "Grafikler, 'Genç' kitlenin sayısal üstünlüğünü ve harcama potansiyelini görsel olarak doğrulamaktadır.",
      v2Caption: "Şekil 2: Yaş, Gelir ve Harcama Dağılımları (Rplot02)",
      genderTitle: "Cinsiyet Analizi",
      genderDesc: "Yapılan T-Testi ve MANOVA analizleri sonucunda, cinsiyet ile harcama skoru veya yıllık gelir arasında istatistiksel olarak anlamlı bir fark bulunamamıştır (p > 0.05).",
      genderResult: "Sonuç: Pazarlama stratejilerinde cinsiyet ayrımına gidilmesi veri tarafından desteklenmemektedir.",
      trendTitle: "Segment Bazlı Harcama Eğilimi",
      trendLabels: ["Gençler (~18-35)", "Orta Yaş (36-55)", "Yaşlılar (55+)"],
      trendLevels: ["Yüksek", "Orta", "Düşük"],
      trendNote: "* Analiz raporuna göre en yüksek harcama skoru genç grupta (Grup 1), en yüksek gelir ise orta yaş grubunda (Grup 2) gözlemlenmiştir."
    },
    strategy: {
      mainTitle: "Nihai İş Stratejisi",
      mainDesc: "Elde edilen veriler ışığında, markanın 'gelir artışı varsayımına' dayalı stratejiler yerine 'etkileşim odaklı' stratejilere yönelmesi gerekmektedir.",
      target1Title: "1. Öncelikli Hedef: Gençler",
      target1List: [
        "Strateji: Gelirden bağımsız, cinsiyetsiz yaklaşım.",
        "Aksiyon: 'Gençleri Sevindirme' temalı kampanyalar.",
        "Neden: En yüksek harcama skoru ve marka sadakati potansiyeli bu gruptadır."
      ],
      target2Title: "2. İkincil Hedef: Orta Yaş (36-40)",
      target2List: [
        "Strateji: Hediye ve özel gün odaklı yaklaşım.",
        "Aksiyon: Orta yaş grubunun yüksek alım gücünü, kendileri için değil, çevreleri için harcamaya yönlendirmek.",
        "Neden: En yüksek gelire sahip grup olmalarına rağmen harcama skorları düşüktür; tetikleyiciye ihtiyaçları vardır."
      ],
      avoidTitle: "3. Kaçınılması Gerekenler",
      avoidList: [
        "Hata: Sadece 'Zengin' (Yüksek Gelirli) müşteriye VIP muamelesi yapmak.",
        "Risk: V2 analizi, yüksek gelirin yüksek harcamayı garanti etmediğini kanıtlamıştır. Bu strateji kaynak israfına yol açabilir."
      ]
    },
    footer: "Kaynak: Mall Customer Segmentation Report - Final.pdf & Analysis Scripts"
  }
};

const Dashboard = () => {
  const [activeTab, setActiveTab] = useState('overview');
  const [lang, setLang] = useState('en'); // Default language set to English
  const t = translations[lang]; // Current language object

  // Summary statistics data
  const stats = {
    totalCustomers: 200,
    avgAge: "38.9",
    avgIncome: "59.8k $",
    avgSpending: "50.2"
  };

  return (
    <div className="dashboard-container">
      {/* Language Switcher */}
      <div className="lang-switch-container">
        <button 
          className={`lang-btn ${lang === 'tr' ? 'active' : ''}`} 
          onClick={() => setLang('tr')}>TR</button>
        <button 
          className={`lang-btn ${lang === 'en' ? 'active' : ''}`} 
          onClick={() => setLang('en')}>EN</button>
      </div>

      {/* Header */}
      <header className="header">
        <h1>{t.title}</h1>
        <p>{t.subtitle}</p>
      </header>

      {/* Stats Row */}
      <div className="stats-row">
        <div className="stat-box">
          <div className="stat-val">{stats.totalCustomers}</div>
          <div className="stat-label">{t.stats.total}</div>
        </div>
        <div className="stat-box">
          <div className="stat-val">{stats.avgAge}</div>
          <div className="stat-label">{t.stats.age}</div>
        </div>
        <div className="stat-box">
          <div className="stat-val">{stats.avgIncome}</div>
          <div className="stat-label">{t.stats.income}</div>
        </div>
        <div className="stat-box">
          <div className="stat-val">{stats.avgSpending}</div>
          <div className="stat-label">{t.stats.score}</div>
        </div>
      </div>

      {/* Navigation Tabs */}
      <div className="tabs">
        <button 
          className={`tab-btn ${activeTab === 'overview' ? 'active' : ''}`} 
          onClick={() => setActiveTab('overview')}>
          {t.tabs.overview}
        </button>
        <button 
          className={`tab-btn ${activeTab === 'analysis' ? 'active' : ''}`} 
          onClick={() => setActiveTab('analysis')}>
          {t.tabs.analysis}
        </button>
        <button 
          className={`tab-btn ${activeTab === 'strategy' ? 'active' : ''}`} 
          onClick={() => setActiveTab('strategy')}>
          {t.tabs.strategy}
        </button>
      </div>

      {/* Content Area */}
      <div className="content-area">
        {activeTab === 'overview' && <OverviewView t={t} />}
        {activeTab === 'analysis' && <AnalysisView t={t} />}
        {activeTab === 'strategy' && <StrategyView t={t} />}
      </div>

      <footer className="footer-cite">
        {t.footer}
      </footer>
    </div>
  );
};

// Overview Component
const OverviewView = ({ t }) => {
  return (
    <div className="card-grid">
      <div className="card">
        <h3>{t.overview.goalTitle}</h3>
        <p>{t.overview.goalDesc}</p>
      </div>
      <div className="card">
        <h3>{t.overview.mythTitle}</h3>
        <p dangerouslySetInnerHTML={{ __html: t.overview.mythDesc }} />
      </div>
      <div className="card" style={{ borderLeftColor: '#27ae60' }}>
        <h3>{t.overview.realityTitle}</h3>
        <p>{t.overview.realityDesc}</p>
      </div>
    </div>
  );
};

// Analysis Component
const AnalysisView = ({ t }) => {
  return (
    <div>
      <div className="card-grid">
        {/* Methodology Table */}
        <div className="card">
          <h3>{t.analysis.compTitle}</h3>
          <table className="comparison-table">
            <thead>
              <tr>
                {t.analysis.tableHead.map((h, i) => <th key={i}>{h}</th>)}
              </tr>
            </thead>
            <tbody>
              {t.analysis.rows.map((row, i) => (
                <tr key={i}>
                  {row.map((cell, j) => <td key={j}>{cell}</td>)}
                </tr>
              ))}
            </tbody>
          </table>
        </div>

        {/* Cluster Chart */}
        <div className="card">
          <h3>{t.analysis.v1Title}</h3>
          <p>{t.analysis.v1Desc}</p>
          <img src={clusterChart} alt="K-Means Cluster Chart" className="chart-img" />
          <div className="img-caption">{t.analysis.v1Caption}</div>
        </div>
      </div>

      {/* Distribution Charts */}
      <div className="card-grid">
        <div className="card" style={{gridColumn: '1 / -1'}}>
           <h3>{t.analysis.v2Title}</h3>
           <div style={{display: 'flex', flexWrap: 'wrap', gap: '20px', alignItems: 'center'}}>
             <div style={{flex: '1 1 400px'}}>
               <p>{t.analysis.v2Intro}</p>
               <ul className="strategy-list">
                 {t.analysis.v2List.map((item, i) => <li key={i}>{item}</li>)}
               </ul>
               <div className="highlight-box">
                 {t.analysis.v2Highlight}
               </div>
             </div>
             <div style={{flex: '1 1 400px'}}>
               <img src={distChart} alt="Distribution Charts" className="chart-img" />
               <div className="img-caption">{t.analysis.v2Caption}</div>
             </div>
           </div>
        </div>
      </div>

      {/* Gender Analysis */}
      <div className="card-grid">
        <div className="card">
          <h3>{t.analysis.genderTitle}</h3>
          <p dangerouslySetInnerHTML={{__html: t.analysis.genderDesc}} />
          <div className="highlight-box" style={{backgroundColor: '#fff3cd', color: '#856404'}}>
            {t.analysis.genderResult}
          </div>
        </div>

         {/* Visual Bars */}
         <div className="card">
          <h3>{t.analysis.trendTitle}</h3>
          <div className="bar-chart-container">
            {[85, 50, 40].map((width, i) => (
              <div className="bar-row" key={i}>
                <span className="bar-label">{t.analysis.trendLabels[i]}</span>
                <div className="bar-wrapper">
                  <div className="bar-fill" style={{width: `${width}%`, background: i === 0 ? 'var(--accent-color)' : '#95a5a6'}}>
                    {t.analysis.trendLevels[i]}
                  </div>
                </div>
              </div>
            ))}
          </div>
          <p style={{marginTop: '15px', fontSize: '0.9rem', color: '#666'}}>
            {t.analysis.trendNote}
          </p>
        </div>
      </div>
    </div>
  );
};

// Strategy Component
const StrategyView = ({ t }) => {
  return (
    <div className="card-grid">
      <div className="card" style={{gridColumn: '1 / -1'}}>
        <h3>{t.strategy.mainTitle}</h3>
        <p>{t.strategy.mainDesc}</p>
      </div>

      <div className="card">
        <h3>{t.strategy.target1Title}</h3>
        <ul className="strategy-list">
          {t.strategy.target1List.map((item, i) => <li key={i} dangerouslySetInnerHTML={{__html: item}} />)}
        </ul>
      </div>

      <div className="card">
        <h3>{t.strategy.target2Title}</h3>
        <ul className="strategy-list">
          {t.strategy.target2List.map((item, i) => <li key={i} dangerouslySetInnerHTML={{__html: item}} />)}
        </ul>
      </div>

      <div className="card">
        <h3>{t.strategy.avoidTitle}</h3>
        <ul className="strategy-list">
          {t.strategy.avoidList.map((item, i) => <li key={i} dangerouslySetInnerHTML={{__html: item}} />)}
        </ul>
      </div>
    </div>
  );
};

export default Dashboard;
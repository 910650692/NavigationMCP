package com.sgm.navi.hmi.permission;

import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import com.android.utils.NetWorkUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogUseReminderBinding;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/31
 */
public class ReminderDialog extends BaseFullScreenDialog<DialogUseReminderBinding> {

    private WebView mWebView;

    public ReminderDialog(Context context, IBaseDialogClickListener baseDialogClickListener) {
        super(context);
        setDialogClickListener(baseDialogClickListener);
    }

    @Override
    protected DialogUseReminderBinding initLayout() {
        return DialogUseReminderBinding.inflate(getLayoutInflater());
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mWebView = mViewBinding.reminderDetail.reminderWebView;
        configureWebView();

        setCancelable(false);
        mViewBinding.reminderIndex.reminderTermsService.setOnClickListener(new View.OnClickListener() {
            @Override
            @HookMethod(eventName = BuryConstant.EventName.AMAP_SERVICEAGREEMENT_CHECK)
            public void onClick(View v) {
                showOrHideDetail(true);
                mViewBinding.reminderDetail.reminderTitle.setText(R.string.reminder_page_service_title);

                if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                    boolean isDarkMode = ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext());
                    mViewBinding.reminderDetail.netErrorHint.setVisibility(View.INVISIBLE);
                    Logger.d("ReminderDialog", "isDarkModeEnabled: ", isDarkMode);
                    String serviceTermsUrl = isDarkMode ?
                            getContext().getString(R.string.service_terms_url_dark) :
                            getContext().getString(R.string.service_terms_url_light);

                    mWebView.loadUrl(serviceTermsUrl);
                } else {
                    Logger.d("ReminderDialog", "Network is not available, cannot load service terms.");
                    mViewBinding.reminderDetail.netErrorHint.setVisibility(View.VISIBLE);
                    mViewBinding.reminderDetail.reminderWebView.setVisibility(View.INVISIBLE);
                }
            }
        });
        mViewBinding.reminderIndex.reminderPagePrivacy.setOnClickListener(v -> {
            showOrHideDetail(true);
            mViewBinding.reminderDetail.reminderTitle.setText(R.string.reminder_page_privacy_title);

            if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                boolean isDarkMode = ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext());
                mViewBinding.reminderDetail.netErrorHint.setVisibility(View.INVISIBLE);
                Logger.d("ReminderDialog", "isDarkModeEnabled: ", isDarkMode);
                String privacyPolicyUrl = isDarkMode ?
                        getContext().getString(R.string.privacy_policy_url_dark) :
                        getContext().getString(R.string.privacy_policy_url_light);
                mWebView.loadUrl(privacyPolicyUrl);
            } else {
                Logger.d("ReminderDialog", "Network is not available, cannot load service terms.");
                mViewBinding.reminderDetail.netErrorHint.setVisibility(View.VISIBLE);
                mViewBinding.reminderDetail.reminderWebView.setVisibility(View.INVISIBLE);
            }
        });
        mViewBinding.reminderIndex.dialogCommit.setOnClickListener(v -> {
            if (mDialogClickListener != null) {
                mDialogClickListener.onCommitClick();
            }
            cancel();
        });
        mViewBinding.reminderIndex.dialogCancel.setOnClickListener(v -> {
            if (mDialogClickListener != null) {
                mDialogClickListener.onCancelClick();
            }
            cancel();
        });

        mViewBinding.reminderDetail.reminderIvBack.setOnClickListener(v -> {
            showOrHideDetail(false);
        });
        mViewBinding.getRoot().setOnClickListener(v -> {
            FloatViewManager.getInstance().hideAllCardWidgets( false);
        });
    }

    private void showOrHideDetail(boolean isShow) {
        if (isShow) {
            mViewBinding.reminderIndex.reminderRootIndex.setVisibility(View.INVISIBLE);
            mViewBinding.reminderDetail.reminderRootDetail.setVisibility(View.VISIBLE);
        } else {
            mViewBinding.reminderIndex.reminderRootIndex.setVisibility(View.VISIBLE);
            mViewBinding.reminderDetail.reminderRootDetail.setVisibility(View.INVISIBLE);
            mWebView.stopLoading();
            mWebView.setVisibility(View.GONE);
        }
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        setDialogClickListener(null);
    }

    private void configureWebView() {
        WebSettings webSettings = mWebView.getSettings();
        webSettings.setJavaScriptEnabled(true);
        webSettings.setDomStorageEnabled(true);
        webSettings.setCacheMode(WebSettings.LOAD_DEFAULT);
        mWebView.setWebViewClient(new WebViewClient() {
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request) {
                Logger.d("ReminderDialog", "shouldOverrideUrlLoading: ", request.getUrl().toString());
                view.loadUrl(request.getUrl().toString());
                return true;
            }

            @Override
            public void onPageStarted(WebView view, String url, Bitmap favicon) {
                super.onPageStarted(view, url, favicon);
                view.setVisibility(View.INVISIBLE);
            }

            @Override
            public void onPageFinished(WebView view, String url) {
                super.onPageFinished(view, url);
                applyCustomStyles(view);
                view.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        view.setVisibility(View.VISIBLE);
                        view.animate().alpha(1f).setDuration(200).start();
                    }
                }, 350);
            }

            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                Logger.d("ReminderDialog", "deprecate shouldOverrideUrlLoading: ", url);
                view.loadUrl(url);
                return true;
            }
        });
    }

    private void applyCustomStyles(WebView view) {
        view.setBackgroundColor(0x00000000);

        StringBuilder cssBuilder = new StringBuilder();
        cssBuilder.append("javascript:(function() {")
                .append("var style = document.createElement('style');")
                .append("style.type = 'text/css';")
                .append("style.id = 'custom-style';"); // 添加ID便于管理

        final boolean isDark = ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext());
        Logger.d("ReminderDialog", "isDarkModeEnabled: ", isDark);
        if (!isDark) {
            cssBuilder.append("style.innerHTML = '")
                    .append("html, body { ")
                    .append("background: transparent !important; ")
                    .append("background-color: transparent !important; ")
                    .append("} ")
                    .append("body, div, p, span, h1, h2, h3, h4, h5, h6, li, td, th, article, section { ")
                    .append("background-color: transparent !important; ")
                    .append("background: transparent !important; ")
                    .append("color: #333333 !important; ")
                    .append("font-family: monospace !important; ")
                    .append("font-weight: 400 !important; ")
                    .append("font-size: 28px !important; ")
                    .append("line-height: 1.5 !important; ")
                    .append("} ")
                    .append("a, a:link, a:visited, a:hover, a:active { ")
                    .append("color: #245fea !important; ")
                    .append("font-weight: 400 !important; ")
                    .append("background: transparent !important; ")
                    .append("} ")
                    .append("';");
        } else {
            cssBuilder.append("style.innerHTML = '")
                    .append("html, body { ")
                    .append("background: transparent !important; ")
                    .append("background-color: transparent !important; ")
                    .append("} ")
                    .append("body, div, p, span, h1, h2, h3, h4, h5, h6, li, td, th, article, section { ")
                    .append("background-color: transparent !important; ")
                    .append("background: transparent !important; ")
                    .append("color: #f0f0f0 !important; ")
                    .append("font-family: monospace !important; ")
                    .append("font-weight: 400 !important; ")
                    .append("font-size: 28px !important; ")
                    .append("line-height: 1.5 !important; ")
                    .append("} ")
                    .append("a, a:link, a:visited, a:hover, a:active { ")
                    .append("color: #2461ea !important; ")
                    .append("font-weight: 400 !important; ")
                    .append("background: transparent !important; ")
                    .append("} ")
                    .append("';");
        }

        cssBuilder.append("var existingStyle = document.getElementById('custom-style');")
                .append("if (existingStyle) existingStyle.remove();")
                .append("document.head.appendChild(style);")
                .append("})()");

        view.evaluateJavascript(cssBuilder.toString(), null);

        // 额外的样式强制应用
        view.evaluateJavascript(
                "javascript:" +
                        "document.documentElement.style.background='transparent';" +
                        "document.body.style.background='transparent';" +
                        "document.body.style.fontSize='28px';" +
                        "document.body.style.fontWeight='400';" +
                        "document.body.style.color='" + (!ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext()) ? "#333333" : "#f0f0f0") + "';",
                null);
    }
}

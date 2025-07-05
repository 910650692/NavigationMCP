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
import com.android.utils.log.Logger;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogUseReminderBinding;
import com.sgm.navi.hmi.launcher.FloatViewManager;
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
                    boolean isDarkMode = isDarkModeEnabled();
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
                boolean isDarkMode = isDarkModeEnabled();
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
        webSettings.setForceDark(WebSettings.FORCE_DARK_ON);
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

                String darkModeCSS = "javascript:(function() {" +
                        "var style = document.createElement('style');" +
                        "style.type = 'text/css';" +
                        "style.innerHTML = 'body { background-color: #121212; color: #ffffff; }';" +
                        "document.head.appendChild(style);" +
                        "})()";

                if (isDarkModeEnabled()) {
                    view.evaluateJavascript(darkModeCSS, null);
                }
                view.setVisibility(View.VISIBLE);
            }

            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                Logger.d("ReminderDialog", "deprecate shouldOverrideUrlLoading: ", url);
                view.loadUrl(url);
                return true;
            }
        });
    }

    private boolean isDarkModeEnabled() {
        return (getContext().getResources().getConfiguration().uiMode &
                Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
    }
}

package com.sgm.navi.ui.view.refresh;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.ui.R;
import com.sgm.navi.ui.view.SkinFrameLayout;
import com.sgm.navi.ui.view.SkinImageView;
import com.sgm.navi.ui.view.SkinTextView;

public class SettingHeadRefreshView extends SkinFrameLayout implements HeadView {
    private static final String TAG = SettingHeadRefreshView.class.getSimpleName();

    private SkinTextView mSkinTextView;
    private SkinImageView mSkinImageView;
    private ProgressBar mProgressBar;
    private String mTips = "下拉刷新";
    private boolean mRefresh = true;

    public SettingHeadRefreshView(final Context context) {
        this(context, null);
    }

    public SettingHeadRefreshView(final Context context, final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SettingHeadRefreshView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    /**
     * 初始化
     *
     * @param context 上下文
     */
    private void init(final Context context) {
        final View view = LayoutInflater.from(context).inflate(R.layout.layout_setting_header, null);
        addView(view);
        if (view != null) {
            mSkinTextView = view.findViewById(R.id.header_tv);
            mSkinImageView = view.findViewById(R.id.header_arrow);
            mProgressBar = view.findViewById(R.id.header_progress);
        }
    }

    @Override
    public void begin() {
        if (mSkinImageView != null) {
            mSkinImageView.setVisibility(VISIBLE);
        }
        if (mProgressBar != null) {
            mProgressBar.setVisibility(GONE);
        }
        safetySetText(mSkinTextView, mTips);
    }

    @Override
    public void progress(final float progress, final float all) {
        final float time = progress / all;
        if (progress >= all - 10) {
            if (mRefresh) {
                safetySetText(mSkinTextView, "松手即可刷新");
            }
        } else {
            safetySetText(mSkinTextView, mTips);
        }
    }

    @Override
    public void finishing(final float progress, final float all) {

    }

    @Override
    public void loading() {
        if (mSkinImageView != null) {
            mSkinImageView.setVisibility(GONE);
        }
        if (mProgressBar != null) {
            mProgressBar.setVisibility(VISIBLE);
        }
        if (mRefresh) {
            safetySetText(mSkinTextView, "刷新中");
        }
    }

    @Override
    public void normal() {
        if (mSkinImageView != null) {
            mSkinImageView.setVisibility(VISIBLE);
        }
        if (mProgressBar != null) {
            mProgressBar.setVisibility(GONE);
        }
        safetySetText(mSkinTextView, mTips);
    }

    @Override
    public void setRefresh(final boolean isRefresh) {
        this.mRefresh = isRefresh;
    }

    @Override
    public View getView() {
        return this;
    }

    @Override
    public void setRefreshTips(final String tips) {
        mTips = tips;
    }

    private void safetySetText(TextView textView, String string) {
        if (ConvertUtils.isEmpty(textView)) {
            Logger.e(TAG, "safetySetText textView == null");
            return;
        }
        textView.setText(string);
    }
}

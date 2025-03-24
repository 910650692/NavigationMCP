package com.fy.navi.ui.view.refresh;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ProgressBar;

import com.fy.navi.ui.R;
import com.fy.navi.ui.view.SkinFrameLayout;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

public class HeadRefreshView extends SkinFrameLayout implements HeadView {

    private SkinTextView mSkinTextView;
    private SkinImageView mSkinImageView;
    private ProgressBar mProgressBar;
    private String mTips = "下拉刷新";
    private boolean mRefresh = true;

    public HeadRefreshView(final Context context) {
        this(context, null);
    }

    public HeadRefreshView(final Context context, final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public HeadRefreshView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    /**
     * 初始化
     *
     * @param context 上下文
     */
    private void init(final Context context) {
        final View view = LayoutInflater.from(context).inflate(R.layout.layout_header, null);
        addView(view);
        mSkinTextView = view.findViewById(R.id.header_tv);
        mSkinImageView = view.findViewById(R.id.header_arrow);
        mProgressBar = view.findViewById(R.id.header_progress);
    }

    @Override
    public void begin() {
        mSkinImageView.setVisibility(VISIBLE);
        mProgressBar.setVisibility(GONE);
        mSkinTextView.setText(mTips);
    }

    @Override
    public void progress(final float progress, final float all) {
        final float time = progress / all;
        if (time >= 0.9f) {
            if (mRefresh) {
                mSkinImageView.setRotation(180);
            }
        } else {
            mSkinImageView.setRotation(0);
        }
        if (progress >= all - 10) {
            if (mRefresh) {
                mSkinTextView.setText("回到上一页");
            }
        } else {
            mSkinTextView.setText(mTips);
        }
    }

    @Override
    public void finishing(final float progress, final float all) {

    }

    @Override
    public void loading() {
        mSkinImageView.setVisibility(GONE);
        mProgressBar.setVisibility(VISIBLE);
        if (mRefresh) {
            mSkinTextView.setText("刷新中...");
        }
    }

    @Override
    public void normal() {
        mSkinImageView.setVisibility(VISIBLE);
        mProgressBar.setVisibility(GONE);
        mSkinTextView.setText(mTips);
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
}

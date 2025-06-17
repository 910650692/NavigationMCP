package com.fy.navi.ui.view.refresh;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ProgressBar;

import com.android.utils.log.Logger;
import com.fy.navi.ui.R;
import com.fy.navi.ui.view.SkinFrameLayout;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

public class LoadMoreView extends SkinFrameLayout implements FooterView {

    private SkinTextView mSkinTextView;
    private SkinImageView mSkinImageView;
    private ProgressBar mProgressBar;
    private String mTips = "上拉加载";
    private boolean mCanLoadMore = true;

    public LoadMoreView(final Context context) {
        this(context, null);
    }

    public LoadMoreView(final Context context, final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LoadMoreView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    /**
     * 初始化
     *
     * @param context Context
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

    }

    @Override
    public void progress(final float progress, final float all) {
        final float s = progress / all;
        if (s >= 0.9f) {
            mSkinImageView.setRotation(0);
        } else {
            mSkinImageView.setRotation(180);
        }
        if (progress >= all - 10) {
            if (mCanLoadMore) {
                if (mTips.contains("没有下一页了")) {
                    if(Logger.openLog) {
                        Logger.d("已经没有下一页了");
                    }
                } else {
                    mSkinTextView.setText("加载更多");
                }
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
        if (mTips.contains("没有下一页了")) {
            return;
        }
        mSkinImageView.setVisibility(GONE);
        mProgressBar.setVisibility(VISIBLE);
        mSkinTextView.setText("加载中...");
    }

    @Override
    public void normal() {
        mSkinImageView.setVisibility(VISIBLE);
        mProgressBar.setVisibility(GONE);
        mSkinTextView.setText(mTips);
    }

    @Override
    public View getView() {
        return this;
    }

    @Override
    public void setLoadMoreTips(final String tips) {
        mTips = tips;
    }

    @Override
    public void canLoadMore(final boolean canLoadMore) {
        this.mCanLoadMore = canLoadMore;
    }
}

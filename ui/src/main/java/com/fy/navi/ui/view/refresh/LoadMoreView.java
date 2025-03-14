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


/**
 * 上拉加载view
 */
public class LoadMoreView extends SkinFrameLayout implements FooterView {

    private SkinTextView tv;
    private SkinImageView arrow;
    private ProgressBar progressBar;
    private String mTips = "上拉加载";
    private boolean canLoadMore = true;

    public LoadMoreView(Context context) {
        this(context, null);
    }

    public LoadMoreView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public LoadMoreView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    private void init(Context context) {
        View view = LayoutInflater.from(context).inflate(R.layout.layout_header, null);
        addView(view);
        tv = view.findViewById(R.id.header_tv);
        arrow = view.findViewById(R.id.header_arrow);
        progressBar = view.findViewById(R.id.header_progress);
    }

    @Override
    public void begin() {

    }

    @Override
    public void progress(float progress, float all) {
        float s = progress / all;
        if (s >= 0.9f) {
            arrow.setRotation(0);
        } else {
            arrow.setRotation(180);
        }
        if (progress >= all - 10) {
            if (canLoadMore) {
                if (mTips.contains("没有下一页了")) {
                } else {
                    tv.setText("加载更多");
                }
            }
        } else {
            tv.setText(mTips);
        }
    }

    @Override
    public void finishing(float progress, float all) {

    }

    @Override
    public void loading() {
        if (mTips.contains("没有下一页了")) {
            return;
        }
        arrow.setVisibility(GONE);
        progressBar.setVisibility(VISIBLE);
        tv.setText("加载中...");
    }

    @Override
    public void normal() {
        arrow.setVisibility(VISIBLE);
        progressBar.setVisibility(GONE);
        tv.setText(mTips);
    }

    @Override
    public View getView() {
        return this;
    }

    @Override
    public void setLoadMoreTips(String tips) {
        mTips = tips;
    }

    @Override
    public void canLoadMore(boolean canLoadMore) {
        this.canLoadMore = canLoadMore;
    }
}

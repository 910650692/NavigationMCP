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
 * 下拉刷新
 */
public class HeadRefreshView extends SkinFrameLayout implements HeadView {

    private SkinTextView tv;
    private SkinImageView arrow;
    private ProgressBar progressBar;
    private String mTips = "下拉刷新";
    private boolean isRefresh = true;

    public HeadRefreshView(Context context) {
        this(context, null);
    }

    public HeadRefreshView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public HeadRefreshView(Context context, AttributeSet attrs, int defStyleAttr) {
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
        arrow.setVisibility(VISIBLE);
        progressBar.setVisibility(GONE);
        tv.setText(mTips);
    }

    @Override
    public void progress(float progress, float all) {
        float s = progress / all;
        if (s >= 0.9f) {
            if (isRefresh) {
                arrow.setRotation(180);
            }
        } else {
            arrow.setRotation(0);
        }
        if (progress >= all - 10) {
            if (isRefresh) {
                tv.setText("回到上一页");
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
        arrow.setVisibility(GONE);
        progressBar.setVisibility(VISIBLE);
        if (isRefresh) {
            tv.setText("刷新中...");
        }
    }

    @Override
    public void normal() {
        arrow.setVisibility(VISIBLE);
        progressBar.setVisibility(GONE);
        tv.setText(mTips);
    }

    @Override
    public void setRefresh(boolean isRefresh) {
        this.isRefresh = isRefresh;
    }

    @Override
    public View getView() {
        return this;
    }

    @Override
    public void setRefreshTips(String tips) {
        mTips = tips;
    }
}

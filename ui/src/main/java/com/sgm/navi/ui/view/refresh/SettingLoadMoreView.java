package com.sgm.navi.ui.view.refresh;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ProgressBar;

import com.android.utils.log.Logger;
import com.sgm.navi.ui.R;
import com.sgm.navi.ui.view.SkinFrameLayout;
import com.sgm.navi.ui.view.SkinImageView;
import com.sgm.navi.ui.view.SkinTextView;

public class SettingLoadMoreView extends SkinFrameLayout implements FooterView {

    private SkinTextView mSkinTextView;
    private SkinImageView mSkinImageView;
    private String mTips = "已加载全部行程";
    private boolean mCanLoadMore = true;

    public SettingLoadMoreView(final Context context) {
        this(context, null);
    }

    public SettingLoadMoreView(final Context context, final AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SettingLoadMoreView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context);
    }

    /**
     * 初始化
     *
     * @param context Context
     */
    private void init(final Context context) {
        final View view = LayoutInflater.from(context).inflate(R.layout.layout_setting_footer, null);
        addView(view);
        mSkinTextView = view.findViewById(R.id.header_tv);
        mSkinImageView = view.findViewById(R.id.header_arrow);
    }

    @Override
    public void begin() {

    }

    @Override
    public void progress(final float progress, final float all) {
        final float s = progress / all;
        if (progress >= all - 10) {
            if (mCanLoadMore) {
                if (Logger.openLog) {
                    Logger.d("已加载全部行程");
                }
                mSkinTextView.setText("已加载全部行程");
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
        mSkinTextView.setText("已加载全部行程");
    }

    @Override
    public void normal() {
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

package com.fy.navi.hmi.traffic;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import androidx.fragment.app.FragmentActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.DialogTrafficBigPicBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;

import java.util.List;

/**
 * Author: QiuYaWei
 * Date: 2025/3/1
 * Description: [大图详情页面]
 */
public class BigPicDetailDialog extends BaseFullScreenDialog<DialogTrafficBigPicBinding> {
    private List<String> mPics;
    private PicPageAdapter picPageAdapter;
    private FragmentActivity fragmentActivity;

    protected BigPicDetailDialog(FragmentActivity activity, List<String> pics) {
        super(activity);
        this.mPics = pics;
        this.fragmentActivity = activity;
    }

    @Override
    protected DialogTrafficBigPicBinding initLayout() {
        return DialogTrafficBigPicBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.tvNumber.setText(String.valueOf(mPics.size()));
        picPageAdapter = new PicPageAdapter(fragmentActivity, mPics);
        mViewBinding.viewPage.setAdapter(picPageAdapter);
        mViewBinding.viewPage.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                mViewBinding.ivPre.setAlpha(position <= 0 ? 0.5f : 1f);
                mViewBinding.ivNext.setAlpha(position >= mPics.size() - 1 ? 0.5f : 1f);
            }
        });
        mViewBinding.ivClose.setOnClickListener(v -> {
            dismiss();
        });
        mViewBinding.ivPre.setOnClickListener(v -> {
            int index = mViewBinding.viewPage.getCurrentItem();
            if (index > 0) {
                mViewBinding.viewPage.setCurrentItem(index - 1);
            }
        });
        mViewBinding.ivNext.setOnClickListener(v -> {
            int index = mViewBinding.viewPage.getCurrentItem();
            if (index + 1 < mPics.size()) {
                mViewBinding.viewPage.setCurrentItem(index + 1);
            }
        });

        boolean flag = mPics.size() > 1;
        mViewBinding.ivNext.setVisibility(flag ? View.VISIBLE : View.INVISIBLE);
        mViewBinding.ivPre.setVisibility(flag ? View.VISIBLE : View.INVISIBLE);
        mViewBinding.tvNumber.setVisibility(flag ? View.VISIBLE : View.INVISIBLE);
    }
}

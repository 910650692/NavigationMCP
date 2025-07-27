package com.sgm.navi.hmi.traffic;

import android.content.Context;
import android.content.res.Resources;
import android.os.Bundle;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;

import androidx.fragment.app.FragmentActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.android.utils.ScreenUtils;
import com.sgm.navi.hmi.databinding.DialogTrafficBigPicBinding;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;

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
    private int dialogWidth, dialogHeight;
    protected BigPicDetailDialog(FragmentActivity activity, List<String> pics) {
        super(activity);
        this.mPics = pics;
        this.fragmentActivity = activity;
        calculateSize();
    }

    @Override
    protected DialogTrafficBigPicBinding initLayout() {
        return DialogTrafficBigPicBinding.inflate(LayoutInflater.from(getContext()));
    }

    private void calculateSize() {
        final Context context = ScreenUtils.Companion.getInstance().getTargetDisplayContext(getContext(), 0);
        final Resources resources = context.getResources();
        final float marginHor = resources.getDimension(com.sgm.navi.ui.R.dimen.traffic_big_pic_margin_hor)*2;
        final float marginVertical = resources.getDimension(com.sgm.navi.ui.R.dimen.traffic_big_pic_margin_vertical)*2;
        dialogWidth = (int) (ScreenUtils.Companion.getInstance().getRealScreenWidth(context) - marginHor);
        dialogHeight = (int) (ScreenUtils.Companion.getInstance().getRealScreenHeight(context) -marginVertical);
    }

    @Override
    public void show() {
        super.show();
        // 隐藏
        BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (baseActivity instanceof MapActivity) {
            MapActivity activity = (MapActivity) baseActivity;
            activity.hideOrShowFragmentContainer(false);
        }
        WindowManager.LayoutParams layoutParams = getWindow().getAttributes();
        layoutParams.width = dialogWidth;
        layoutParams.height = dialogHeight;
        layoutParams.gravity = Gravity.CENTER;
        getWindow().setAttributes(layoutParams);
    }

    @Override
    public void dismiss() {
        super.dismiss();
        // 显示
        BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (baseActivity instanceof MapActivity) {
            MapActivity activity = (MapActivity) baseActivity;
            activity.hideOrShowFragmentContainer(true);
        }
    }

    @Override
    public void cancel() {
        super.cancel();
        BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (baseActivity instanceof MapActivity) {
            MapActivity activity = (MapActivity) baseActivity;
            activity.hideOrShowFragmentContainer(true);
        }
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

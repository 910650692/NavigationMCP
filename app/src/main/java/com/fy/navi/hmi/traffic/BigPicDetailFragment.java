package com.fy.navi.hmi.traffic;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.ViewOutlineProvider;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.ImageView;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentPicDetailBinding;
import com.fy.navi.ui.action.OnImageLoadListener;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;

/**
 * Author: QiuYaWei
 * Date: 2025/3/1
 * Description: [在这里描述文件功能]
 */
public class BigPicDetailFragment extends BaseFragment<FragmentPicDetailBinding, BaseBigPicDetailViewModel> implements OnImageLoadListener {
    public static final String KEY_URL = "url_key";
    private String url;
    private Animation animation;
    // only for test
//    private String testUrl = "https://img-blog.csdnimg.cn/fcc22710385e4edabccf2451d5f64a99.jpeg";
    private static final String TAG = "BigPicDetailFragment";

    public static BigPicDetailFragment newInstance(String url) {
        BigPicDetailFragment fragment = new BigPicDetailFragment();
        Bundle bundle = new Bundle();
        bundle.putString(KEY_URL, url);
        fragment.setArguments(bundle);
        return fragment;
    }

    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);
        if (getArguments().containsKey(KEY_URL)) {
            url = getArguments().getString(KEY_URL);
        }
        animation = AnimationUtils.loadAnimation(context, R.anim.rotate_animation);
    }

    @Override
    public int onLayoutId() {
        return R.layout.fragment_pic_detail;
    }

    @Override
    public int onInitVariableId() {
        return 0;
    }

    @Override
    public void onInitView() {
        Logger.i(TAG, "onLoadStart");
        // 修改依据：[https://peedp.saic-gm.com/ccm/web/projects/VCS_Info4.0_High_Platform_PATAC_RTC#action=com.ibm.team.workitem.viewWorkItem&id=1055757]
        mBinding.ivPic.setScaleType(ImageView.ScaleType.FIT_XY);
        mBinding.tvRetry.setOnClickListener(v -> {
            onInitData();
        });
        mBinding.ivPic.setClipToOutline(true);
        // 创建一个圆角矩形的轮廓提供器
        mBinding.ivPic.setOutlineProvider(new ViewOutlineProvider() {
            @Override
            public void getOutline(View view, android.graphics.Outline outline) {
                int radius = 20; // 圆角半径
                outline.setRoundRect(0, 0, view.getWidth(), view.getHeight(), radius);
            }
        });
    }

    @Override
    public void onInitData() {
        ViewAdapterKt.loadImageUrl(mBinding.ivPic, url, this);
    }

    @Override
    public void onLoadCompleted(boolean isSuccess) {
        if (isSuccess) {
            mBinding.ivPic.setVisibility(View.VISIBLE);
            mBinding.ivLoading.setVisibility(View.GONE);
            mBinding.tvLoading.setVisibility(View.GONE);
            mBinding.tvRetry.setVisibility(View.GONE);
        } else {
            mBinding.ivPic.setVisibility(View.GONE);
            mBinding.ivLoading.setVisibility(View.GONE);
            mBinding.tvLoading.setVisibility(View.VISIBLE);
            mBinding.tvRetry.setVisibility(View.VISIBLE);
            mBinding.tvLoading.setText(R.string.limit_load_fail);
        }
    }

    @Override
    public void onLoadStart() {
        mBinding.tvLoading.setText(R.string.limit_loading);
        mBinding.ivLoading.setAnimation(animation);
        mBinding.ivLoading.startAnimation(animation);
        mBinding.ivLoading.setVisibility(View.VISIBLE);
        mBinding.tvLoading.setVisibility(View.VISIBLE);
        mBinding.ivPic.setVisibility(View.GONE);
        mBinding.tvRetry.setVisibility(View.GONE);
        animation.start();
    }
}

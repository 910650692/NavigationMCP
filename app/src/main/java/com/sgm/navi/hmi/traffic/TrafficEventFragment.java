package com.sgm.navi.hmi.traffic;

import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import android.graphics.Color;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.view.ViewOutlineProvider;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;

import androidx.databinding.Observable;
import androidx.databinding.library.baseAdapters.BR;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentTrafficDetailBinding;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.aos.FyGSubTraEventDetail;
import com.sgm.navi.service.define.aos.TrafficType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.MapTypeManager;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.ui.action.OnImageLoadListener;
import com.sgm.navi.ui.action.ViewAdapterKt;
import com.sgm.navi.ui.base.BaseFragment;

import java.util.Objects;

/**
 * Author: QiuYaWei
 * Date: 2025/2/27
 * Description: [在这里描述文件功能]
 */
public class TrafficEventFragment extends BaseFragment<FragmentTrafficDetailBinding, TrafficViewModel>
        implements OnImageLoadListener {
    private static final String TAG = "TrafficEventFragment";
    private Animation animation;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_traffic_detail;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        animation = AnimationUtils.loadAnimation(getContext(), R.anim.rotate_animation);
        mBinding.loadingLayout.setViewModel(mViewModel);

        mViewModel.uiState.addOnPropertyChangedCallback(new Observable.OnPropertyChangedCallback() {
            @Override
            public void onPropertyChanged(Observable sender, int propertyId) {
                showOrHideLoading(mViewModel.uiState.get() == TrafficEventUiState.LOADING);
            }
        });
        mViewModel.preEnable.addOnPropertyChangedCallback(new Observable.OnPropertyChangedCallback() {
            @Override
            public void onPropertyChanged(Observable sender, int propertyId) {
                mBinding.ivPre.setEnabled(mViewModel.preEnable.get());
            }
        });
        mViewModel.nextEnable.addOnPropertyChangedCallback(new Observable.OnPropertyChangedCallback() {
            @Override
            public void onPropertyChanged(Observable sender, int propertyId) {
                mBinding.ivNext.setEnabled(mViewModel.nextEnable.get());
            }
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
        Bundle bundle = getArguments();
        if (bundle != null) {
            PoiInfoEntity entity = bundle.getParcelable(AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_ENTITY);
            boolean isNeedConvert = bundle.getBoolean(
                    AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_IS_NEED_CONVERT, true);
            if (entity != null && mViewModel != null) {
                mViewModel.queryTrafficEvent(entity, isNeedConvert);
            }
        }
    }

    public void showOrHideLoading(boolean isShow) {
        if (isShow) {
            mBinding.loadingLayout.ivLoading.startAnimation(animation);
        } else {
            animation.cancel();
            animation.reset();
        }
    }

    public void updateUi(FyGSubTraEventDetail subTraEventDetail) {
        mBinding.tvDesc.setText(subTraEventDetail.head);
        updateRoadIconAndName(subTraEventDetail.layertag);
        updatePic(subTraEventDetail.picurl);
    }

    private void updateRoadIconAndName(int layerTag) {
        Logger.i(TAG, "updateRoadIconAndName:" + layerTag);
        mBinding.ivIcon.setImageResource(TrafficType.getIconByTrafficType(layerTag));
        mBinding.tvType.setText(TrafficType.getTitleByTrafficType(layerTag));
    }

    public String getTrafficType(){
        return mBinding.tvType.getText().toString();
    }

    private void updatePic(String picUrl) {
        Logger.i(TAG, "picUrl:" + picUrl);
        if (TextUtils.isEmpty(picUrl)) {
            mBinding.ivPic.setImageDrawable(null);
            mBinding.layoutLoadingImg.setVisibility(View.GONE);
            mBinding.ivPic.setVisibility(View.GONE);
            return;
        }
        startAnimation();
        ViewAdapterKt.loadImageUrl(mBinding.ivPic, picUrl, this);
    }

    @Override
    public void onResume() {
        super.onResume();
        notifySceneImmersiveStatus(true);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        animation.cancel();
        notifySceneImmersiveStatus(false);
    }

    /***
     * 通知导航中沉浸态发生改变
     * @param flag true 代表创建完成，false销毁
     */
    public void notifySceneImmersiveStatus(final boolean flag) {
        final boolean isOnNavigating = Objects.equals(NavistatusAdapter.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING);
        if (isOnNavigating) {
            final MapType mapTypeId = MapTypeManager.getInstance().getMapTypeIdByName(mScreenId);
            final ImersiveStatus status = flag ? ImersiveStatus.TOUCH : ImersiveStatus.IMERSIVE;
            Logger.d(TAG, MAP_TOUCH, "mapTypeId:" , mapTypeId, "status:" , status);
            ImmersiveStatusScene.getInstance().setImmersiveStatus(mapTypeId, status);
        }
    }

    public void setIncreaseBtnColor(Boolean isIncrease) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.tvIncrease.setSelected(isIncrease);
            mBinding.tvIncrease.setTextColor(isIncrease ?
                    getResources().getColor(R.color.bg_route_big_window_select) :
                    getResources().getColor(com.sgm.navi.scene.R.color.search_quick_tab_view_color));
        });
    }

    public void setDecreaseBtnColor(Boolean isDecrease) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.tvDecrease.setSelected(isDecrease);
            mBinding.tvDecrease.setTextColor(isDecrease ?
                    getResources().getColor(R.color.bg_route_big_window_select) :
                    getResources().getColor(com.sgm.navi.scene.R.color.search_quick_tab_view_color));
        });
    }

    public void setIncreaseBtnAlpha(float alpha) {
        mBinding.tvIncrease.setAlpha(alpha);
    }

    public void setDecreaseBtnAlpha(float alpha) {
        mBinding.tvDecrease.setAlpha(alpha);
    }

    @Override
    public void onLoadCompleted(boolean isSuccess) {
        if (isSuccess) {
            mBinding.ivPic.setVisibility(View.VISIBLE);
            mBinding.ivLoading.setVisibility(View.GONE);
            mBinding.tvLoading.setVisibility(View.GONE);
            mBinding.tvRetry.setVisibility(View.GONE);
            stopAnimation();
        } else {
            stopAnimation();
            mBinding.ivPic.setVisibility(View.GONE);
            mBinding.tvRetry.setVisibility(View.VISIBLE);
            mBinding.tvLoading.setText(R.string.limit_load_fail);
        }
    }

    @Override
    public void onLoadStart() {

    }

    private void startAnimation() {
        mBinding.layoutLoadingImg.setVisibility(View.VISIBLE);
        mBinding.tvLoading.setText(R.string.limit_loading);
        mBinding.ivLoading.setAnimation(animation);
        mBinding.ivLoading.startAnimation(animation);
        mBinding.ivLoading.setVisibility(View.VISIBLE);
        mBinding.tvLoading.setVisibility(View.VISIBLE);
        mBinding.tvRetry.setVisibility(View.GONE);
        mBinding.ivPic.setVisibility(View.GONE);
        animation.start();
    }

    private void stopAnimation() {
        mBinding.ivLoading.clearAnimation();
        mBinding.ivLoading.setVisibility(View.GONE);
        animation.cancel();
    }
}

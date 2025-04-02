package com.fy.navi.hmi.traffic;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.view.ViewOutlineProvider;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;

import androidx.databinding.Observable;
import androidx.databinding.library.baseAdapters.BR;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentTrafficDetailBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.aos.FyGSubTraEventDetail;
import com.fy.navi.service.define.aos.TrafficType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;

import java.util.Objects;

/**
 * Author: QiuYaWei
 * Date: 2025/2/27
 * Description: [在这里描述文件功能]
 */
public class TrafficEventFragment extends BaseFragment<FragmentTrafficDetailBinding, BaseTrafficViewModel> {
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
            if (entity != null) {
                mViewModel.queryTrafficEvent(entity);
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

    private void updatePic(String picUrl) {
        Logger.i(TAG, "picUrl:" + (TextUtils.isEmpty(picUrl)));
        if (TextUtils.isEmpty(picUrl)) return;
        ViewAdapterKt.loadImageUrl(mBinding.ivPic, picUrl, com.fy.navi.scene.R.drawable.test_pic, com.fy.navi.scene.R.drawable.test_pic);
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
        final boolean isOnNavigating = Objects.equals(MapPackage.getInstance().getNaviStatus(), NaviStatus.NaviStatusType.NAVING);
        if (isOnNavigating) {
            final MapType mapTypeId = MapTypeManager.getInstance().getMapTypeIdByName(mScreenId);
            final ImersiveStatus status = flag ? ImersiveStatus.TOUCH : ImersiveStatus.IMERSIVE;
            ImmersiveStatusScene.getInstance().setImmersiveStatus(mapTypeId, status);
        }
    }
}

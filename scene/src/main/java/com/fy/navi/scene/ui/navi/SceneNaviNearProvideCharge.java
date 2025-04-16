package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.CallSuper;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.databinding.NaviSceneNearProvideStationChargeBinding;
import com.fy.navi.scene.impl.navi.SceneNaviNearProvideChargeImpl;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.ui.view.SwipeDeleteLayout;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/31
 * Description: [充电桩]
 */
public class SceneNaviNearProvideCharge extends NaviSceneBase<NaviSceneNearProvideStationChargeBinding, SceneNaviNearProvideChargeImpl> {
    private static final String TAG = "SceneNaviNearProvideCharge";
    private PoiInfoEntity mPoiInfoEntity;
    private SearchResultEntity mEntity;

    public SceneNaviNearProvideCharge(@NonNull Context context) {
        super(context);
    }

    public SceneNaviNearProvideCharge(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviNearProvideCharge(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    @CallSuper
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_PROVIDE_CHARGE;
    }

    @Override
    protected NaviSceneNearProvideStationChargeBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return NaviSceneNearProvideStationChargeBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviNearProvideChargeImpl initSceneImpl() {
        return new SceneNaviNearProvideChargeImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        mViewBinding.viewNaviNow.setOnClickListener(v -> {
            startNaviRightNow();
            notifySceneStateChange(false);
        });
        mViewBinding.viewBg.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mISceneCallback) && !ConvertUtils.isNull(mEntity)) {
                Logger.i(TAG, "showRecChargeList");
                notifySceneStateChange(false);
                mISceneCallback.showRecChargeList(mEntity);
            } else {
                Logger.e(TAG, "data or callBack is null!");
            }
        });
        mViewBinding.swipeDeleteView.setOnSwipeActionListener(new SwipeDeleteLayout.OnSwipeActionListener() {
            @Override
            public void onDelete(SwipeDeleteLayout layout) {
                notifySceneStateChange(false);
            }

            @Override
            public void onStateChanged(boolean onDragging) {
                if (onDragging) {
                    cancelCountdown();
                } else {
                    startCountdown();
                }
            }
        });
    }

    /***
     * 立即导航到推荐的目的地，这里导航到充电站
     */
    private void startNaviRightNow() {
        if (mISceneCallback != null && mPoiInfoEntity != null) {
            mISceneCallback.startNaviRightNow(mPoiInfoEntity);
            mISceneCallback.updateSceneVisible(getSceneId(), false);
        } else {
            Logger.w(TAG, "startNaviRightNow failed, callback or entity is null!");
        }
    }

    /***
     * 更新UI
     * @param searchResultEntity
     * TODO 其它信息：比如快充和慢充数量需要付费才可以拿到，暂时字段为null
     */
    public void updateUi(final SearchResultEntity searchResultEntity) {
        ThreadManager.getInstance().postUi(() -> {
            mEntity = searchResultEntity;
            final boolean isBadData = ConvertUtils.isNull(searchResultEntity) || ConvertUtils.isEmpty(searchResultEntity.getPoiList());
            Logger.i(TAG, "updateUi:" + getSceneName(), "isBadData:" + isBadData);
            if (isBadData) {
                Logger.i(TAG, "updateUi failed, entity is null or empty!");
                return;
            }
            getNaviSceneEvent().notifySceneStateChange(INaviSceneEvent.SceneStateChangeType.SceneShowState, getSceneId());
            mPoiInfoEntity = searchResultEntity.getPoiList().get(0);
            mViewBinding.tvTitle.setText(mPoiInfoEntity.getName());
            mViewBinding.tvDistance.setText(mPoiInfoEntity.getDistance());
            // 获取充电站信息
            if (!ConvertUtils.isEmpty(mPoiInfoEntity.getChargeInfoList())) {
                final ChargeInfo chargeInfo = mPoiInfoEntity.getChargeInfoList().get(0);
                setStationInfo(chargeInfo);
            }
            // 清楚搜索扎标
            OpenApiHelper.clearSearchLabelMark();
        });
    }

    /***
     * 导航中更新推荐充电桩显示距离
     */
    public void updateDistance() {
        if (mViewBinding == null || getVisibility() == GONE) {
            return;
        }
        if (mPoiInfoEntity == null || mPoiInfoEntity.getPoint() == null) {
            return;
        }
        //Logger.d(TAG, "updateDistance", "distance:"+mScreenViewModel.getChargeDistance(mPoiInfoEntity.getPoint()));
        mViewBinding.tvDistance.setText(mScreenViewModel.getChargeDistance(mPoiInfoEntity.getPoint()));
    }

    private void setStationInfo(ChargeInfo chargeInfo) {
        if (chargeInfo == null) {
            return;
        }
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_PROVIDE_CHARGE, true);
        }
        Logger.d(TAG, "setStationInfo", "size:" + chargeInfo.getFast_total() + "_" + chargeInfo.getFast_free() + "_" + chargeInfo.getSlow_total());
        final boolean haQuickStation = chargeInfo.getFast_total() > 0;
        final boolean hasLowerSlowStation = chargeInfo.getSlow_total() > 0;
        mViewBinding.layoutChargeDesc.root.setVisibility((!hasLowerSlowStation && !haQuickStation) ? View.GONE : View.VISIBLE);
        if (haQuickStation) {
            mViewBinding.layoutChargeDesc.root.setVisibility(View.VISIBLE);
            mViewBinding.layoutChargeDesc.tvQuickUse.setText(String.valueOf(chargeInfo.getFast_free()));
            mViewBinding.layoutChargeDesc.tvQuickTotal.setText("/" + chargeInfo.getFast_total());
            mViewBinding.layoutChargeDesc.tvQuickUse.setVisibility(View.VISIBLE);
            mViewBinding.layoutChargeDesc.tvQuickTotal.setVisibility(View.VISIBLE);
        } else {
            mViewBinding.layoutChargeDesc.tvQuickUse.setVisibility(View.GONE);
            mViewBinding.layoutChargeDesc.tvQuickTotal.setVisibility(View.GONE);
        }

        if (hasLowerSlowStation) {
            mViewBinding.layoutChargeDesc.root.setVisibility(View.VISIBLE);
            mViewBinding.layoutChargeDesc.tvSlowUse.setText(String.valueOf(chargeInfo.getSlow_free()));
            mViewBinding.layoutChargeDesc.tvSlowTotal.setText("/" + chargeInfo.getSlow_total());
            mViewBinding.layoutChargeDesc.tvSlowUse.setVisibility(View.VISIBLE);
            mViewBinding.layoutChargeDesc.tvSlowTotal.setVisibility(View.VISIBLE);
        } else {
            mViewBinding.layoutChargeDesc.tvSlowUse.setVisibility(View.GONE);
            mViewBinding.layoutChargeDesc.tvSlowTotal.setVisibility(View.GONE);
        }

        if (isPlentiful(chargeInfo)) {
            mViewBinding.layoutChargeDesc.tvTense.setVisibility(View.VISIBLE);
        } else {
            mViewBinding.layoutChargeDesc.tvTense.setVisibility(View.GONE);
        }
    }

    /***
     * UE-2.6-1
     * -充电位紧张：总充电位数<=30个，剩余充电位<30% ；总充电位数>30个，剩余充电位<10% 或 剩余充电位少于10个。
     * @return true 紧张 false 充足
     */
    private boolean isPlentiful(final ChargeInfo chargeInfo) {
        final int totalSize = chargeInfo.getFast_total() + chargeInfo.getSlow_total();
        final int freeTotal = chargeInfo.getFast_free() + chargeInfo.getSlow_free();
        return totalSize <= 30 || (totalSize > 30 && freeTotal < 10);
    }

    @Override
    public boolean isNeedAutoStartTimer() {
        return true;
    }
}

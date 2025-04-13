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
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.NaviSceneNearProvideStationGasBinding;
import com.fy.navi.scene.impl.navi.SceneNaviNearProvideGasImpl;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.view.SwipeDeleteLayout;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/31
 * Description: [加油站]
 */
public class SceneNaviNearProvideGas extends NaviSceneBase<NaviSceneNearProvideStationGasBinding, SceneNaviNearProvideGasImpl> {
    private static final String TAG = "SceneNaviNearProvideGas";
    private SearchResultEntity mEntity;

    public SceneNaviNearProvideGas(@NonNull Context context) {
        super(context);
    }

    public SceneNaviNearProvideGas(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviNearProvideGas(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    @CallSuper
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_PROVIDE_GAS;
    }

    @Override
    protected NaviSceneNearProvideStationGasBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return NaviSceneNearProvideStationGasBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviNearProvideGasImpl initSceneImpl() {
        return new SceneNaviNearProvideGasImpl(this);
    }

    @Override
    public void startCountdown() {
        super.startCountdown();
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        mViewBinding.ivIcon.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mISceneCallback) && !ConvertUtils.isNull(mEntity)) {
                Logger.i(TAG, "showRecGasList");
                notifySceneStateChange(false);
                mISceneCallback.showRecGasList(mEntity);
            } else {
                Logger.e(TAG, "data or callBack is null!");
            }
        });
        mViewBinding.viewNaviNow.setOnClickListener(v -> {
            startNaviRightNow();
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
        if (!ConvertUtils.isNull(mISceneCallback) && !ConvertUtils.isNull(mEntity) && !ConvertUtils.isEmpty(mEntity.getPoiList())) {
            mISceneCallback.startNaviRightNow(mEntity.getPoiList().get(0));
            if (!ConvertUtils.isNull(mScreenViewModel)) {
                notifySceneStateChange(false);
            }
        } else {
            Logger.w(TAG, "startNaviRightNow failed, callback or entity is null!");
        }
    }

    /***
     * 更新UI
     * @param searchResultEntity
     *
     */
    public void updateUi(final SearchResultEntity searchResultEntity) {
        this.mEntity = searchResultEntity;
        ThreadManager.getInstance().postUi(() -> {
            Logger.i(TAG, "updateUi");
            if (ConvertUtils.isNull(searchResultEntity) || ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                Logger.i(TAG, "updateUi failed, entity is null or empty!");
            } else {
                final PoiInfoEntity poiInfo = searchResultEntity.getPoiList().get(0);
                mViewBinding.tvTitle.setText(poiInfo.getName());
                mViewBinding.tvDistance.setText(poiInfo.getDistance());
                // 取前两个类型和价格
                setGasTypeAndPrice(poiInfo.getStationList());
                notifySceneStateChange(true);
            }
        });
    }

    private void setGasTypeAndPrice(final List<GasStationInfo> infos) {
        Logger.i(TAG, "setGasTypeAndPrice-size:" + (ConvertUtils.isNull(infos) ? 0 : infos.size()));
        if (ConvertUtils.isEmpty(infos)) {
            mViewBinding.llTypeAndPrice.setVisibility(ViewGroup.GONE);
        } else if (infos.size() >= 2) {
            mViewBinding.llTypeAndPrice.setVisibility(ViewGroup.VISIBLE);
            final GasStationInfo firstBean = infos.get(0);
            final GasStationInfo secondBean = infos.get(1);
            mViewBinding.tv95.setVisibility(View.VISIBLE);
            mViewBinding.tv95.setText(firstBean.getType());
            mViewBinding.tv95Price.setText(firstBean.getPrice());

            mViewBinding.tv92.setVisibility(View.VISIBLE);
            mViewBinding.tv92.setText(secondBean.getType());
            mViewBinding.tv92Price.setText(secondBean.getPrice());
        } else if (infos.size() == 1) {
            final GasStationInfo firstBean = infos.get(0);
            mViewBinding.tv95.setVisibility(View.VISIBLE);
            mViewBinding.tv95.setText(firstBean.getType());
            mViewBinding.tv95Price.setText(firstBean.getPrice());

            mViewBinding.tv92.setVisibility(View.GONE);
        }
    }
}

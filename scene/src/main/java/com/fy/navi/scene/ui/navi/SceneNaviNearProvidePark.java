package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.CallSuper;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.databinding.NaviSceneNearProvideStationParkBinding;
import com.fy.navi.scene.impl.navi.SceneNaviNearProvideParkImpl;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.ui.view.SwipeDeleteLayout;

import java.util.ArrayList;
import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/31
 * Description: [停车场]
 */
public class SceneNaviNearProvidePark extends NaviSceneBase<NaviSceneNearProvideStationParkBinding, SceneNaviNearProvideParkImpl> {
    private static final String TAG = "SceneNaviNearProvidePark";
    private ArrayList<NaviParkingEntity> mList = new ArrayList<>();
    public SceneNaviNearProvidePark(@NonNull Context context) {
        super(context);
    }

    public SceneNaviNearProvidePark(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviNearProvidePark(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    @CallSuper
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_PROVIDE_PARK;
    }

    @Override
    protected NaviSceneNearProvideStationParkBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return NaviSceneNearProvideStationParkBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviNearProvideParkImpl initSceneImpl() {
        return new SceneNaviNearProvideParkImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        mViewBinding.ivIcon.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mISceneCallback)) {
                mISceneCallback.showRecParkList(mList);
                notifySceneStateChange(false);
            }
        });

        mViewBinding.viewNaviNow.setOnClickListener(v -> {
            if (!ConvertUtils.isNull(mScreenViewModel) && !ConvertUtils.isEmpty(mList)) {
                final NaviParkingEntity entity = mList.get(0);
                mScreenViewModel.startNaviRightNow(NaviDataFormatHelper.getPoiInfoEntity(entity));
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

    /**
     * @param list list
     */
    public void updateUi(final List<NaviParkingEntity> list) {
        ThreadManager.getInstance().postUi(() -> {
            if (ConvertUtils.isEmpty(list)) {
                return;
            }
            mList.clear();
            mList.addAll(list);
            final NaviParkingEntity firstBean = list.get(0);
            Logger.i(TAG, "updateUi", "size:" + firstBean.getSpaceTotal() + "-" + firstBean.getSpaceFree());
            mViewBinding.tvTitle.setText(firstBean.getName());
            mViewBinding.tvDistance.setText(firstBean.getDistance());
            if (firstBean.getSpaceTotal()>0) {
                mViewBinding.tvFreeSize.setText(String.valueOf(firstBean.getSpaceFree()));
                mViewBinding.tvTotalSize.setText("/" + firstBean.getSpaceTotal());
            }
            notifySceneStateChange(true);
        });
    }

    /***
     *
     * @param naviEtaInfo
     */
    public void onUpdateNaviInfo(final NaviEtaInfo naviEtaInfo, final MapType mapType) {
        setScreenId(mapType);
        if (mScreenViewModel != null) {
            mScreenViewModel.checkParking(naviEtaInfo);
        }
    }

    @Override
    public boolean isNeedAutoStartTimer() {
        return true;
    }
}

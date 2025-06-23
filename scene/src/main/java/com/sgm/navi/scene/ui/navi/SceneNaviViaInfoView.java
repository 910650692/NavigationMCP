package com.sgm.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviViaInfoViewBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviViaInfoImpl;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.navi.NaviEtaInfo;

/**
 * 显示途径点信息、达到状态
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviViaInfoView extends NaviSceneBase<SceneNaviViaInfoViewBinding, SceneNaviViaInfoImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_VIA_INFO;

    public SceneNaviViaInfoView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviViaInfoView(@NonNull final Context context,
                                @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaInfoView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_VIA_DETAIL_INFO;
    }

    @Override
    protected SceneNaviViaInfoViewBinding createViewBinding(final LayoutInflater inflater,
                                                            final ViewGroup viewGroup) {
        return SceneNaviViaInfoViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviViaInfoImpl initSceneImpl() {
        return new SceneNaviViaInfoImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviViaInfo(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    /**
     * @param naviEtaInfo 导航信息
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviEtaInfo);
        }
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        if (mScreenViewModel != null) {
            mScreenViewModel.addCallBack(sceneCallback);
        }
    }

    /**
     * 更新途经点到达状态
     * @param str 途经点名称
     * @param listSize 0
     */
    public void onArriveVia(final String str) {
        Logger.i(TAG, "onArriveVia: ", str);
        mViewBinding.stvWay.setText(str);
        mViewBinding.stvWayArrive.setVisibility(VISIBLE);
        mViewBinding.stvViaCount.setVisibility(GONE);
    }

    /**
     * 更新页面途经点显示信息
     * @param viaName 途经点名称
     * @param totalSize 途经点总数量
     */
    public void updateViaInfo(final String viaName, final int totalSize) {
        Logger.i(TAG, "updateViaInfo: ", viaName);

        boolean isViaArrived = mISceneCallback != null && mISceneCallback.getIsViaArrived();
        boolean showViaCount = totalSize > 1;

        mViewBinding.stvWay.setText(viaName);

        if (isViaArrived) {
            mViewBinding.stvWayArrive.setVisibility(VISIBLE);
            mViewBinding.stvViaCount.setVisibility(GONE);
        } else {
            mViewBinding.stvWayArrive.setVisibility(GONE);
            mViewBinding.stvViaCount.setVisibility(showViaCount ? VISIBLE : GONE);
        }
        if (showViaCount) {
            mViewBinding.stvViaCount.setText(
                    getResources().getString(R.string.navi_via_count, totalSize)
            );
        }
    }


    /**
     * @param viaIndex 途经点索引
     */
    public void onUpdateViaPass(final long viaIndex) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onUpdateViaPass(viaIndex);
        }
    }

    /**
     * 开始导航
     */
    public void startNavigation() {
        mScreenViewModel.startNavigation();
    }

    public void refreshViaInfo() {
        if (mScreenViewModel != null) {
            mScreenViewModel.refreshViaInfo();
        }
    }
}

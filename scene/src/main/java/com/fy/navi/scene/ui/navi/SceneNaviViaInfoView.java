package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviViaInfoViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviViaInfoImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;

/**
 * 显示途径点信息、达到状态
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviViaInfoView extends NaviSceneBase<SceneNaviViaInfoViewBinding, SceneNaviViaInfoImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    public static final int ONLY_NAME = 0;
    public static final int WITH_ARRIVED_TAG = 1;
    public static final int WITH_COUNT_TAG = 2;

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
    protected NaviSceneId getSceneId() {
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
            mScreenViewModel.addSceneCallback(sceneCallback);
        }
    }

    /**
     * 更新途经点到达状态
     * @param str 途经点名称
     * @param listSize 0
     */
    public void onArriveVia(final String str, final int listSize) {
        Logger.i(TAG, "onArriveVia: " + str);
        mViewBinding.stvWay.setText(str);
        mViewBinding.stvWayArrive.setVisibility(VISIBLE);
        mViewBinding.stvViaCount.setVisibility(INVISIBLE);
    }

    /**
     * 更新页面途经点显示信息
     * @param viaName 途经点名称
     * @param totalSize 途经点总数量
     */
    public void updateViaInfo(final String viaName, final int totalSize) {
        Logger.i(TAG, "updateViaInfo: " + viaName);
        mViewBinding.stvWayArrive.setVisibility(INVISIBLE);
        mViewBinding.stvWay.setText(viaName);
        if (totalSize > 1) {
            mViewBinding.stvViaCount.setVisibility(VISIBLE);
            mViewBinding.stvViaCount.setText(String.format(getResources().
                    getString(R.string.navi_via_count), totalSize));
        } else {
            mViewBinding.stvViaCount.setVisibility(INVISIBLE);
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
}

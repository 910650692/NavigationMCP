package com.fy.navi.scene.ui.navi;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviParkListViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviParkListImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.adapter.NaviParkListAdapter;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;

import java.util.List;

/**
 * 列表scene 停车场列表
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviParkListView extends NaviSceneBase<SceneNaviParkListViewBinding, SceneNaviParkListImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private NaviParkListAdapter mNaviParkListAdapter;
    private ISceneCallback mISceneCallback;

    public SceneNaviParkListView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviParkListView(@NonNull final Context context,
                                 @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviParkListView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                 final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_PARK_LIST;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(
                NaviSceneId.NAVI_SCENE_PARK_LIST, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_PARK_LIST, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_PARK_LIST, true);
        }
    }

    @Override
    protected SceneNaviParkListViewBinding createViewBinding(final LayoutInflater inflater,
                                                             final ViewGroup viewGroup) {
        return SceneNaviParkListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviParkListImpl initSceneImpl() {
        return new SceneNaviParkListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviParkList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        Logger.d(TAG, "SceneNaviListView initObserver");
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.srvAddVia.setLayoutManager(layoutManager);

        mNaviParkListAdapter = new NaviParkListAdapter();
        mViewBinding.srvAddVia.setAdapter(mNaviParkListAdapter);
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        if (mScreenViewModel != null) {
            mScreenViewModel.addSceneCallback(sceneCallback);
        }
    }

    /**
     * @param list list
     * @param isCheck isCheck
     * @param select 是否选择
     */
    public void showNaviParkList(final List<NaviParkingEntity> list, final boolean isCheck,
                                 final int select) {
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        mNaviParkListAdapter.setOnItemClickListener(mScreenViewModel);
        mScreenViewModel.showParkingMark(select);
        if (isCheck) {
            for (int i = 0; i < list.size(); i++) {
                final NaviParkingEntity naviParkingEntity = list.get(i);
                if (naviParkingEntity.isEndPoi) {
                    notifyList(List.of(naviParkingEntity), select);
                    return;
                }
            }
        }
        Logger.i(TAG, "SceneNaviParkListImpl list：" + list.size());
        notifyList(list, select);
    }

    /**
     * @param list list
     * @param select select
     */
    public void notifyList(final List<NaviParkingEntity> list, final int select) {
        mNaviParkListAdapter.notifyList(list, select);
    }

    /**
     * @param naviEtaInfo 导航信息
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.checkParking(naviEtaInfo);
        }
    }
}

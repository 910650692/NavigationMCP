package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_VIA_POINT_UNFOLD;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.api.navi.INaviViaItemClickListener;
import com.fy.navi.scene.databinding.SceneNaviViaListViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviViaListImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.NaviViaListAdapter;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseFragment;

import java.util.List;

/***终点、途径点列表***/
public class SceneNaviViaListView extends NaviSceneBase<SceneNaviViaListViewBinding, SceneNaviViaListImpl> {
    private static final String TAG = "SceneNaviViaListView";
    private NaviViaListAdapter mNaviViaListAdapter;

    public SceneNaviViaListView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviViaListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_VIA_POINT_UNFOLD;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_VIA_POINT_UNFOLD, this);
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_VIA_POINT_FOLD, false);
        }
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_VIA_POINT_UNFOLD, true);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_VIA_POINT_FOLD, false);
        }
    }

    public void updateViaListState(boolean isExpand) {
        isViaListExpand = isExpand;
        INaviSceneEvent.SceneStateChangeType type;
        NaviSceneId sceneId;
        if (isViaListExpand) {
            type = INaviSceneEvent.SceneStateChangeType.SceneShowState;
            sceneId = NaviSceneId.NAVI_SCENE_VIA_POINT_UNFOLD;
        } else {
            type = INaviSceneEvent.SceneStateChangeType.SceneCloseState;
            sceneId = NaviSceneId.NAVI_SCENE_VIA_POINT_FOLD;
        }
        Logger.i("lvww", "type -> " + type, "sceneId -> " + sceneId);
        getNaviSceneEvent().notifySceneStateChange(type, sceneId);
    }

    @Override
    protected SceneNaviViaListViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviViaListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviViaListImpl initSceneImpl() {
        return new SceneNaviViaListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviViaList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        Logger.d(TAG, "SceneNaviListView initObserver");
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.srvAddVia.setLayoutManager(layoutManager);

        mNaviViaListAdapter = new NaviViaListAdapter();
        mViewBinding.srvAddVia.setAdapter(mNaviViaListAdapter);
        mNaviViaListAdapter.setOnItemClickListener(new INaviViaItemClickListener() {
            @Override
            public void onItemClick(int position, NaviViaEntity entity) {
                Logger.i(TAG, "onItemClick:" + position);
                if (mNaviViaListAdapter.getData().size() <= 1) return;
                if (mISceneCallback != null) {
                    Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    PoiInfoEntity poiInfo = new PoiInfoEntity();
                    poiInfo.setPid(entity.getPid());
                    poiInfo.setPoint(entity.getRealPos());
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_MAP_CLICK, poiInfo));
                }
            }

            @Override
            public void onDelClick(int position, NaviViaEntity entity) {
                Logger.i(TAG, "onDelClick", "position:" + position, "callBack is null :" + (mISceneCallback == null));
                if (mISceneCallback != null) {
                    Logger.i(TAG, "onDelClick:" + position);
                    mISceneCallback.deleteViaPoint(entity);
                }
            }
        });
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        mScreenViewModel.addSceneCallback(sceneCallback);
    }

    public void showNaviViaList(List<NaviViaEntity> list) {
        if (mScreenViewModel == null) {
            Logger.e(TAG, "mScreenViewModel == null：");
            return;
        }
        if (ConvertUtils.isEmpty(list)) {
            mScreenViewModel.cancelTimer();
            return;
        }
        Logger.i(TAG, "SceneNaviListImpl list：" + list.size());
        mNaviViaListAdapter.notifyList(list);
        mScreenViewModel.initTimer();
    }

    public void notifyDeleteViaPointResult(boolean result, NaviViaEntity entity) {
        Logger.i(TAG, "notifyDeleteViaPointResult:" + result);
        if (result) {
            mNaviViaListAdapter.removeData(entity);
        }
    }
}

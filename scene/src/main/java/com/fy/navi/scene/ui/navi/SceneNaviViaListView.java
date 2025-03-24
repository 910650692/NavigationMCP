package com.fy.navi.scene.ui.navi;

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
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseFragment;

import java.util.List;

/**
 * 终点、途径点列表
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviViaListView extends NaviSceneBase<SceneNaviViaListViewBinding, SceneNaviViaListImpl> {
    private static final String TAG = "SceneNaviViaListView";
    private NaviViaListAdapter mNaviViaListAdapter;

    private ISceneCallback mISceneCallback;

    public SceneNaviViaListView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviViaListView(@NonNull final Context context,
                                @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaListView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_VIA_POINT_UNFOLD;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SCENE_VIA_POINT_UNFOLD, this);
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
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_VIA_POINT_UNFOLD, true);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_VIA_POINT_FOLD, false);
        }
    }

    /**
     * @param isExpand 是否展开
     */
    public void updateViaListState(final boolean isExpand) {
        setIsViaListExpand(isExpand);
        final INaviSceneEvent.SceneStateChangeType type;
        final NaviSceneId sceneId;
        if (isIsViaListExpand()) {
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
    protected SceneNaviViaListViewBinding createViewBinding(final LayoutInflater inflater,
                                                            final ViewGroup viewGroup) {
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
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.srvAddVia.setLayoutManager(layoutManager);

        mNaviViaListAdapter = new NaviViaListAdapter();
        mViewBinding.srvAddVia.setAdapter(mNaviViaListAdapter);
        mNaviViaListAdapter.setOnItemClickListener(new INaviViaItemClickListener() {
            @Override
            public void onItemClick(final int position, final NaviViaEntity entity) {
                Logger.i(TAG, "onItemClick:" + position);
                if (mNaviViaListAdapter.getData().size() <= 1) {
                    return;
                }
                if (mISceneCallback != null) {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    final PoiInfoEntity poiInfo = new PoiInfoEntity();
                    poiInfo.setPid(entity.getPid());
                    poiInfo.setPoint(entity.getRealPos());
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.
                            createPoiDetailsFragment(
                                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                                    AutoMapConstant.PoiType.POI_MAP_CLICK, poiInfo));
                }
            }

            @Override
            public void onDelClick(final int position, final NaviViaEntity entity) {
                Logger.i(TAG, "onDelClick", "position:" + position, "callBack is null :" +
                        (mISceneCallback == null));
                if (mISceneCallback != null) {
                    Logger.i(TAG, "onDelClick:" + position);
                    mISceneCallback.deleteViaPoint(entity);
                }
            }
        });
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        mScreenViewModel.addSceneCallback(sceneCallback);
    }

    /**
     * @param list 途经点列表
     */
    public void showNaviViaList(final List<NaviViaEntity> list) {
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

    /**
     * @param result 删除途经点提醒
     * @param entity entity
     */
    public void notifyDeleteViaPointResult(final boolean result, final NaviViaEntity entity) {
        Logger.i(TAG, "notifyDeleteViaPointResult:" + result);
        if (result) {
            mNaviViaListAdapter.removeData(entity);
        }
    }
}

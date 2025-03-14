package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.route.ISceneRouteDetailsSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteDetailsResultListViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteDetailsResultListImpl;
import com.fy.navi.scene.ui.adapter.RouteDetailsResultAdapter;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteDetailsResultListView extends BaseSceneView<SceneRouteDetailsResultListViewBinding, SceneRouteDetailsResultListImpl> {

    private RouteDetailsResultAdapter mRouteDetailsResultAdapter;
    private List<RouteLineSegmentInfo> mRouteLineSegmentInfos;
    private boolean isAvoid = false;
    private RouteAvoidInfo mRouteAvoidInfo;
    private Hashtable<String, ISceneRouteDetailsSelectCallBack> sceneRouteDetailsSelectCallBackHashtable;

    public SceneRouteDetailsResultListView(@NonNull Context context) {
        super(context);
    }

    public SceneRouteDetailsResultListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteDetailsResultListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteDetailsResultListViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneRouteDetailsResultListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteDetailsResultListImpl initSceneImpl() {
        mRouteLineSegmentInfos = new ArrayList<>();
        sceneRouteDetailsSelectCallBackHashtable = new Hashtable<>();
        mRouteAvoidInfo = new RouteAvoidInfo();
        return new SceneRouteDetailsResultListImpl(this);
    }

    public void registerRouteDeatailsCheckedObserver(String key, ISceneRouteDetailsSelectCallBack callBack) {
        sceneRouteDetailsSelectCallBackHashtable.put(key, callBack);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    public SceneRouteDetailsResultListViewBinding getBinding() {
        return mViewBinding;
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
    }

    private void setupRecyclerView() {
        mRouteDetailsResultAdapter = new RouteDetailsResultAdapter();
        mRouteDetailsResultAdapter.setOnItemCheckedChangeListener(routeAvoidInfo -> {
            mRouteAvoidInfo = routeAvoidInfo;
            for (ISceneRouteDetailsSelectCallBack callBack : sceneRouteDetailsSelectCallBackHashtable.values()) {
                callBack.onRouteDetailsChecked(routeAvoidInfo.getCheckedLeastOne());
            }
        });
        mViewBinding.routeDetailResultList.setAdapter(mRouteDetailsResultAdapter);
    }

    public void notifyRouteDetailsResultList(List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        if (ConvertUtils.isEmpty(routeLineSegmentInfos)) return;
        mRouteLineSegmentInfos = routeLineSegmentInfos;
        mRouteDetailsResultAdapter.setAdapterResult(routeLineSegmentInfos, isAvoid);
    }

    private void closeAllItem() {
        if (mViewBinding.routeDetailResultList.getCount() <= NumberUtils.NUM_0) {
            return;
        }
        for (int t= NumberUtils.NUM_0; t < mViewBinding.routeDetailResultList.getCount(); t++) {
            if (mViewBinding.routeDetailResultList.isGroupExpanded(t)) {
                mViewBinding.routeDetailResultList.collapseGroup(t);
            }
        }
    }

    public void setAvoidStatus(boolean isAvoid) {
        this.isAvoid = isAvoid;
        if (ConvertUtils.isEmpty(mRouteLineSegmentInfos)) return;
        closeAllItem();
        mRouteDetailsResultAdapter.setAdapterResult(mRouteLineSegmentInfos, isAvoid);
    }

    public void setEndPoint(String name) {
        mViewBinding.routeDetailsListTvFooter.setText(AppContext.mContext.getResources().getString(R.string.route_details_list_footer) + name);
    }

    public void startAvoidRoad() {
        mScreenViewModel.startAvoidRoad(mRouteAvoidInfo);
    }
}

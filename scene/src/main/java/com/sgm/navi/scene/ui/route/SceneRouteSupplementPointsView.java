package com.sgm.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.constraintlayout.widget.ConstraintSet;

import com.android.utils.TimeUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneRouteSupplementPointsViewBinding;
import com.sgm.navi.scene.impl.route.SceneRouteSupplementPointsImpl;
import com.sgm.navi.service.define.route.RouteSupplementInfo;
import com.sgm.navi.ui.view.SkinConstraintLayout;
import com.sgm.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;


public class SceneRouteSupplementPointsView extends BaseSceneView<SceneRouteSupplementPointsViewBinding, SceneRouteSupplementPointsImpl> {
    private List<View> mAddView = new ArrayList<>();

    public SceneRouteSupplementPointsView(final Context context) {
        super(context);
    }

    public SceneRouteSupplementPointsView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteSupplementPointsView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteSupplementPointsViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteSupplementPointsViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteSupplementPointsImpl initSceneImpl() {
        return new SceneRouteSupplementPointsImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
    }


    /**
     * 初始化列表
     * */
    private void setupRecyclerView() {


    }

    /**
     * 刷新补能点规划
     * @param routeSupplementInfos 更新参数
     * @param total 总长度
     * */
    public void updateSupplementPointsView(final ArrayList<RouteSupplementInfo> routeSupplementInfos, final float total) {
        if (routeSupplementInfos != null && !routeSupplementInfos.isEmpty()) {
            ThreadManager.getInstance().postUi(() -> {
                final SkinConstraintLayout routeChargeProgressLayout = mViewBinding.routeChargeProgressIcons;
                if (mAddView != null && !mAddView.isEmpty()) {
                    for (int i = 0; i < mAddView.size(); i++) {
                        // 移除子视图
                        routeChargeProgressLayout.removeView(mAddView.get(i));
                    }
                    mAddView.clear();
                }

                mViewBinding.routeChargeTotalMileage.setText(TimeUtils.getInstance().getDistanceMsg((long) total));
                int totalTime = 0;

                final int viewWidth = getResources().getDimensionPixelSize(R.dimen.route_supplement_width);
                final int totalWidth = getResources().getDimensionPixelSize(R.dimen.route_supplement_total);
                final float displayProgress = (float) viewWidth /totalWidth;
                float totalDisplayProgress = 1;
                if (totalWidth - viewWidth > 0) {
                    totalDisplayProgress  = (float) (totalWidth - viewWidth)/totalWidth;
                }

                float lastProgress = 0;

                for (int i = 0; i < routeSupplementInfos.size(); i++) {
                    final RouteSupplementInfo routeSupplementInfo = routeSupplementInfos.get(i);
                    final LayoutInflater inflater = (LayoutInflater) this.getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
                    final View customViewItem = inflater.inflate(R.layout.scene_route_supplement_point, routeChargeProgressLayout, false);
                    customViewItem.setId(View.generateViewId());
                    final SkinTextView distanceText = customViewItem.findViewById(R.id.tv_route_charge);
                    distanceText.setText(routeSupplementInfo.getMUnitDistance());
                    final SkinTextView indexText = customViewItem.findViewById(R.id.tv_route_index);
                    indexText.setText(String.valueOf(i+1));
                    routeChargeProgressLayout.addView(customViewItem);
                    mAddView.add(customViewItem);
                    if (routeSupplementInfo.getMChargeTime() == -1) {
                        totalTime = -1;
                    } else if (totalTime != -1) {
                        totalTime += routeSupplementInfo.getMChargeTime()/60;
                    }

                    final ConstraintSet constraintSet = new ConstraintSet();
                    constraintSet.clone(routeChargeProgressLayout);
                    constraintSet.connect(customViewItem.getId(), ConstraintSet.START, routeChargeProgressLayout.getId(), ConstraintSet.START);
                    constraintSet.connect(customViewItem.getId(), ConstraintSet.END, routeChargeProgressLayout.getId(), ConstraintSet.END);
                    constraintSet.connect(customViewItem.getId(), ConstraintSet.TOP, routeChargeProgressLayout.getId(), ConstraintSet.TOP);
                    float nowProgress = (float) routeSupplementInfo.getMDistance() / total;
                    if (lastProgress == 0 || nowProgress > lastProgress + displayProgress) {
                        distanceText.setVisibility(View.VISIBLE);
                        lastProgress = nowProgress;
                    } else {
                        distanceText.setVisibility(View.GONE);
                    }

                    //判断是否和总里程文言重叠
                    if (nowProgress > totalDisplayProgress) {
                        distanceText.setVisibility(View.GONE);
                    }
                    constraintSet.setHorizontalBias(customViewItem.getId(),  nowProgress);
                    constraintSet.applyTo(routeChargeProgressLayout);
                }

                if (totalTime == -1) {
                    mViewBinding.routeSupplementTotalMileage.setText(String.format(getResources().
                                    getString(R.string.route_supplement_info), String.valueOf(routeSupplementInfos.size()) ,"-"));
                } else {
                    mViewBinding.routeSupplementTotalMileage.setText(String.format(getResources().
                            getString(R.string.route_supplement_info), String.valueOf(routeSupplementInfos.size()) ,String.valueOf(totalTime)));
                }

            });
        }
    }

}

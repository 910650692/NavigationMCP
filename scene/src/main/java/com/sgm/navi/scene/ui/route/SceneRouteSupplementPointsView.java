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
import java.util.HashSet;
import java.util.List;
import java.util.Set;


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

                mViewBinding.routeChargeTotalMileage.setText(TimeUtils.getInstance().getDistanceString((long) total));
                int totalTime = 0;

                List<View> allViews = new ArrayList<>();
                List<SkinTextView> distanceTextViews = new ArrayList<>();

                final int dotWidthPx = getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_26);

                for (int i = 0; i < routeSupplementInfos.size(); i++) {
                    final RouteSupplementInfo routeSupplementInfo = routeSupplementInfos.get(i);
                    final LayoutInflater inflater = (LayoutInflater) this.getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
                    final View customViewItem = inflater.inflate(R.layout.scene_route_supplement_point, routeChargeProgressLayout, false);
                    customViewItem.setId(View.generateViewId());
                    final SkinTextView distanceText = customViewItem.findViewById(R.id.tv_route_charge);
                    distanceText.setText(routeSupplementInfo.getMUnitDistance());
                    final SkinTextView indexText = customViewItem.findViewById(R.id.tv_route_index);
                    indexText.setText(String.valueOf(i+1));
                    customViewItem.setVisibility(View.INVISIBLE); // 创建所有视图但不显示
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
                    constraintSet.setHorizontalBias(customViewItem.getId(),  ((float) routeSupplementInfo.getMDistance() / total));
                    constraintSet.applyTo(routeChargeProgressLayout);

                    allViews.add(customViewItem);
                    distanceTextViews.add(distanceText);
                }

                routeChargeProgressLayout.post(() -> {
                    Set<Integer> keepIndices = new HashSet<>();
                    keepIndices.add(0); // 第一个总是保留

                    // 上一个保留的视图位置
                    int[] lastLocation = new int[2];
                    allViews.get(0).getLocationOnScreen(lastLocation);
                    float lastRight = lastLocation[0] + dotWidthPx;

                    for (int i = 1; i < allViews.size(); i++) {
                        int[] currentLocation = new int[2];
                        allViews.get(i).getLocationOnScreen(currentLocation);
                        float currentLeft = currentLocation[0];

                        if (currentLeft >= lastRight) {
                            keepIndices.add(i);
                            lastRight = currentLeft + dotWidthPx;
                        }
                    }

                    for (int i = 0; i < allViews.size(); i++) {
                        if (keepIndices.contains(i)) {
                            allViews.get(i).setVisibility(View.VISIBLE);
                            distanceTextViews.get(i).setVisibility(View.VISIBLE);
                        } else {
                            allViews.get(i).setVisibility(View.GONE);
                            distanceTextViews.get(i).setVisibility(View.GONE);
                        }
                    }

                    // 检查距离文本重叠
                    for (int i = 0; i < distanceTextViews.size(); i++) {
                        if (!keepIndices.contains(i) || distanceTextViews.get(i).getVisibility() != View.VISIBLE) {
                            continue;
                        }

                        int[] textLoc1 = new int[2];
                        distanceTextViews.get(i).getLocationOnScreen(textLoc1);
                        float textRight1 = textLoc1[0] + distanceTextViews.get(i).getWidth();

                        for (int j = i + 1; j < distanceTextViews.size(); j++) {
                            if (!keepIndices.contains(j) || distanceTextViews.get(j).getVisibility() != View.VISIBLE) {
                                continue;
                            }

                            int[] textLoc2 = new int[2];
                            distanceTextViews.get(j).getLocationOnScreen(textLoc2);

                            if (textLoc2[0] < textRight1) {
                                distanceTextViews.get(j).setVisibility(View.GONE);
                            } else {
                                break;
                            }
                        }
                    }
                });

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

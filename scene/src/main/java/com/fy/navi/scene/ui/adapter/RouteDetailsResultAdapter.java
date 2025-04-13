package com.fy.navi.scene.ui.adapter;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckBox;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.impl.route.common.SceneRouteCommonStruct;
import com.fy.navi.scene.impl.route.common.SceneRouteDetailEnumRes;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

public class RouteDetailsResultAdapter extends BaseExpandableListAdapter {
    private List<RouteLineSegmentInfo> mRouteLineSegmentInfos; //
    private boolean mIsAvoid;
    private ItemCheckedChangeListener mListener;
    private Hashtable<Integer, Boolean> mHashtable;

    public RouteDetailsResultAdapter() {
        mRouteLineSegmentInfos = new ArrayList<>();
        mIsAvoid = false;
        mHashtable = new Hashtable<>();
    }

    @Override
    public int getGroupCount() {
        return mRouteLineSegmentInfos.size();
    }

    @Override
    public int getChildrenCount(final int groupPosition) {
        return mRouteLineSegmentInfos.get(groupPosition).getMRouteLineSegmentInfos().size();
    }

    @Override
    public RouteLineSegmentInfo getGroup(final int groupPosition) {
        return mRouteLineSegmentInfos.get(groupPosition);
    }

    @Override
    public RouteLineSegmentInfo getChild(final int groupPosition, final int childPosition) {
        return mRouteLineSegmentInfos.get(groupPosition).getMRouteLineSegmentInfos().get(childPosition);
    }

    @Override
    public long getGroupId(final int groupPosition) {
        return groupPosition;
    }

    @Override
    public long getChildId(final int groupPosition, final int childPosition) {
        return childPosition;
    }

    @Override
    public boolean hasStableIds() {
        return true;
    }

    @Override
    public boolean isChildSelectable(final int groupPosition, final int childPosition) {
        return true;
    }

    /**
     * 初始化列表
     * @param routeLineSegmentInfos 列表数据
     * @param isAvoid 避开
     * */
    public void setAdapterResult(final List<RouteLineSegmentInfo> routeLineSegmentInfos, final boolean isAvoid) {
        this.mRouteLineSegmentInfos = routeLineSegmentInfos;
        this.mIsAvoid = isAvoid;
        for (int i = 0; i < getGroupCount(); i++) {
            mHashtable.put(i, false);
        }
        notifyDataSetChanged();
    }

    @SuppressLint("SetTextI18n")
    @Override
    public View getGroupView(final int groupPosition, final boolean isExpanded, final View convertViews, final ViewGroup parent) {
        final GroupViewHolder holder;
        View convertView = convertViews;
        if (convertView == null) {
            convertView = LayoutInflater.from(AppContext.getInstance().getMContext())
                    .inflate(R.layout.route_details_info_result_parent_item, parent, false);
            holder = new GroupViewHolder(convertView);
            convertView.setTag(holder);
        } else {
            holder = (GroupViewHolder) convertView.getTag();
        }

        setFrontIcon(holder, groupPosition);
        holder.mParentRoadName.setText(getGroup(groupPosition).getMLoadName());
        holder.mParentDescription.setText(getGroup(groupPosition).getMDistance() + " "
                + AppContext.getInstance().getMContext().getResources().getString(R.string.route_details_light_count)
                + getGroup(groupPosition).getMLightCount()
                + AppContext.getInstance().getMContext().getResources().getString(R.string.route_details_light_count_unit));
        holder.mParentUpDowmIcon.setImageResource(isExpanded ? R.drawable.img_route_up : R.drawable.img_route_down);
        return convertView;
    }

    /**
     * 初始化列表
     * @param holder view
     * @param groupPosition 列表索引
     * */
    private void setFrontIcon(final GroupViewHolder holder, final int groupPosition) {
        if (mIsAvoid) {
            holder.mParentTurnIcon.setVisibility(View.GONE);
            holder.mParentCheckBox.setVisibility(View.VISIBLE);
            holder.mParentCheckBox.setChecked(mHashtable.get(groupPosition));

            holder.mParentCheckBox.setOnClickListener(view -> {
                if (ConvertUtils.isEmpty(mListener)) {
                    return;
                }
                mListener.onItemCheckedChange(getRouteAvoidInfo(groupPosition, Boolean.FALSE.equals(mHashtable.get(groupPosition))));
            });
        } else {
            holder.mParentCheckBox.setVisibility(View.GONE);
            holder.mParentTurnIcon.setVisibility(View.VISIBLE);
            holder.mParentTurnIcon.setImageResource(SceneRouteDetailEnumRes.getDrawableEnumName(
                    SceneRouteCommonStruct.RouteDetailsMainAction.get(getGroup(groupPosition).getMIconType())).getDayDrawableId());
        }
    }

    @Override
    public View getChildView(final int groupPosition, final int childPosition, final boolean isLastChild
            , final View convertViews, final ViewGroup parent) {
        final ChildViewHolder holder;
        View convertView = convertViews;
        if (convertView == null) {
            convertView = LayoutInflater.from(AppContext.getInstance().getMContext())
                    .inflate(R.layout.route_details_info_result_child_item, parent, false);
            holder = new ChildViewHolder(convertView);
            convertView.setTag(holder);
        } else {
            holder = (ChildViewHolder) convertView.getTag();
        }

        holder.mChildTurnIcon.setImageResource(SceneRouteDetailEnumRes.getDrawableEnumName(
                SceneRouteCommonStruct.RouteDetailsMainAction.get(getChild(groupPosition, childPosition).getMIconType())).getDayDrawableId());
        holder.mChildDescription.setText(getChild(groupPosition, childPosition).getMDistance() + " "
                + AppContext.getInstance().getMContext().getResources().getString(R.string.route_details_after_to_arrive)
                + getChild(groupPosition, childPosition).getMLoadName());
        return convertView;
    }

    /**
     * 获取避开道路数据
     * @param groupPosition 索引
     * @param isChecked 是否勾选
     * @return 道路数据
     * */
    private RouteAvoidInfo getRouteAvoidInfo(final int groupPosition, final boolean isChecked) {
        mHashtable.put(groupPosition, isChecked);
        final RouteAvoidInfo info = new RouteAvoidInfo();
        boolean checkedLeastOne = false;
        final ArrayList<Long> avoidList = new ArrayList<>();
        for (boolean isCheck : mHashtable.values()) {
            checkedLeastOne |= isCheck;
            if (isCheck) {
                avoidList.addAll(getGroup(groupPosition).getMAvoidList());
            }
        }
        info.setMCheckedLeastOne(checkedLeastOne);
        info.setMAvoidList(avoidList);
        return info;
    }

    public void setOnItemCheckedChangeListener(final ItemCheckedChangeListener listener) {
        this.mListener = listener;
    }

    public interface ItemCheckedChangeListener {
        /**
         * item 勾选回调
         * @param routeAvoidInfo 勾选数据
         * */
        void onItemCheckedChange(final RouteAvoidInfo routeAvoidInfo);
    }

    static class GroupViewHolder {
        private SkinImageView mParentTurnIcon;
        private SkinTextView mParentRoadName;
        private SkinTextView mParentDescription;
        private SkinImageView mParentUpDowmIcon;
        private CheckBox mParentCheckBox;

        GroupViewHolder(final View view) {
            mParentTurnIcon = view.findViewById(R.id.route_detail_info_item_img);
            mParentRoadName = view.findViewById(R.id.route_detail_info_item_road_name);
            mParentDescription = view.findViewById(R.id.route_detail_info_item_description);
            mParentUpDowmIcon = view.findViewById(R.id.route_detail_info_item_img_updown);
            mParentCheckBox = view.findViewById(R.id.route_detail_info_item_cbx);
        }
    }

    static class ChildViewHolder {
        private SkinImageView mChildTurnIcon;
        private SkinTextView mChildDescription;

        ChildViewHolder(final View view) {
            mChildTurnIcon = view.findViewById(R.id.route_detail_info_item_child_img);
            mChildDescription = view.findViewById(R.id.route_detail_info_item_child_description);
        }
    }
}

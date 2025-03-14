package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckBox;
import android.widget.CompoundButton;

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
    private List<RouteLineSegmentInfo> routeLineSegmentInfos; //
    private boolean isAvoid;
    private ItemCheckedChangeListener listener;
    private Hashtable<Integer, Boolean> hashtable;

    public RouteDetailsResultAdapter() {
        routeLineSegmentInfos = new ArrayList<>();
        isAvoid = false;
        hashtable = new Hashtable<>();
    }

    @Override
    public int getGroupCount() {
        return routeLineSegmentInfos.size();
    }

    @Override
    public int getChildrenCount(int groupPosition) {
        return routeLineSegmentInfos.get(groupPosition).getRouteLineSegmentInfos().size();
    }

    @Override
    public RouteLineSegmentInfo getGroup(int groupPosition) {
        return routeLineSegmentInfos.get(groupPosition);
    }

    @Override
    public RouteLineSegmentInfo getChild(int groupPosition, int childPosition) {
        return routeLineSegmentInfos.get(groupPosition).getRouteLineSegmentInfos().get(childPosition);
    }

    @Override
    public long getGroupId(int groupPosition) {
        return groupPosition;
    }

    @Override
    public long getChildId(int groupPosition, int childPosition) {
        return childPosition;
    }

    @Override
    public boolean hasStableIds() {
        return true;
    }

    @Override
    public boolean isChildSelectable(int groupPosition, int childPosition) {
        return true;
    }

    public void setAdapterResult(List<RouteLineSegmentInfo> routeLineSegmentInfos, boolean isAvoid) {
        this.routeLineSegmentInfos = routeLineSegmentInfos;
        this.isAvoid = isAvoid;
        notifyDataSetChanged();
    }

    @Override
    public View getGroupView(int groupPosition, boolean isExpanded, View convertView, ViewGroup parent) {
        GroupViewHolder holder;
        if (convertView == null) {
            convertView = LayoutInflater.from(AppContext.mContext).inflate(R.layout.route_details_info_result_parent_item, parent, false);
            holder = new GroupViewHolder(convertView);
            convertView.setTag(holder);
        } else {
            holder = (GroupViewHolder) convertView.getTag();
        }

        setFrontIcon(holder, groupPosition);
        holder.parentRoadName.setText(getGroup(groupPosition).getLoadName());
        holder.parentDescription.setText(getGroup(groupPosition).getDistance() + " " + AppContext.mContext.getResources().getString(R.string.route_details_light_count) + getGroup(groupPosition).getLightCount() + AppContext.mContext.getResources().getString(R.string.route_details_light_count_unit));
        holder.parentUpDowmIcon.setImageResource(isExpanded ? R.drawable.img_route_up : R.drawable.img_route_down);
        return convertView;
    }

    private void setFrontIcon(GroupViewHolder holder, int groupPosition) {
        if (isAvoid) {
            holder.parentTurnIcon.setVisibility(View.GONE);
            holder.parentCheckBox.setVisibility(View.VISIBLE);
            holder.parentCheckBox.setChecked(false);
            hashtable.put(groupPosition, false);
            holder.parentCheckBox.setOnCheckedChangeListener((CompoundButton compoundButton, boolean isChecked) -> {
                if (ConvertUtils.isEmpty(listener)) return;
                listener.OnItemCheckedChange(getRouteAvoidInfo(groupPosition, isChecked));
            });
        } else {
            holder.parentCheckBox.setVisibility(View.GONE);
            holder.parentTurnIcon.setVisibility(View.VISIBLE);
            holder.parentTurnIcon.setImageResource(SceneRouteDetailEnumRes.getDrawableEnumName(SceneRouteCommonStruct.RouteDetailsMainAction.get(getGroup(groupPosition).getIconType())).getDayDrawableId());
        }
    }

    @Override
    public View getChildView(int groupPosition, int childPosition, boolean isLastChild, View convertView, ViewGroup parent) {
        ChildViewHolder holder;
        if (convertView == null) {
            convertView = LayoutInflater.from(AppContext.mContext).inflate(R.layout.route_details_info_result_child_item, parent, false);
            holder = new ChildViewHolder(convertView);
            convertView.setTag(holder);
        } else {
            holder = (ChildViewHolder) convertView.getTag();
        }

        holder.childTurnIcon.setImageResource(SceneRouteDetailEnumRes.getDrawableEnumName(SceneRouteCommonStruct.RouteDetailsMainAction.get(getChild(groupPosition, childPosition).getIconType())).getDayDrawableId());
        holder.childDescription.setText(getChild(groupPosition, childPosition).getDistance() + " " + AppContext.mContext.getResources().getString(R.string.route_details_after_to_arrive) + getChild(groupPosition, childPosition).getLoadName());
        return convertView;
    }

    private RouteAvoidInfo getRouteAvoidInfo(int groupPosition, boolean isChecked) {
        hashtable.put(groupPosition, isChecked);
        RouteAvoidInfo info = new RouteAvoidInfo();
        boolean checkedLeastOne = false;
        ArrayList<Long> avoidList = new ArrayList<>();
        for (boolean isCheck : hashtable.values()) {
            checkedLeastOne |= isCheck;
            if (isCheck) {
                avoidList.addAll(getGroup(groupPosition).getAvoidList());
            }
        }
        info.setCheckedLeastOne(checkedLeastOne);
        info.setAvoidList(avoidList);
        return info;
    }

    public void setOnItemCheckedChangeListener(ItemCheckedChangeListener listener) {
        this.listener = listener;
    }

    static class GroupViewHolder {
        SkinImageView parentTurnIcon;
        SkinTextView parentRoadName;
        SkinTextView parentDescription;
        SkinImageView parentUpDowmIcon;
        CheckBox parentCheckBox;

        GroupViewHolder(View view) {
            parentTurnIcon = view.findViewById(R.id.route_detail_info_item_img);
            parentRoadName = view.findViewById(R.id.route_detail_info_item_road_name);
            parentDescription = view.findViewById(R.id.route_detail_info_item_description);
            parentUpDowmIcon = view.findViewById(R.id.route_detail_info_item_img_updown);
            parentCheckBox = view.findViewById(R.id.route_detail_info_item_cbx);
        }
    }

    static class ChildViewHolder {
        SkinImageView childTurnIcon;
        SkinTextView childDescription;

        ChildViewHolder(View view) {
            childTurnIcon = view.findViewById(R.id.route_detail_info_item_child_img);
            childDescription = view.findViewById(R.id.route_detail_info_item_child_description);
        }
    }

    public interface ItemCheckedChangeListener {
        void OnItemCheckedChange(RouteAvoidInfo routeAvoidInfo);
    }
}

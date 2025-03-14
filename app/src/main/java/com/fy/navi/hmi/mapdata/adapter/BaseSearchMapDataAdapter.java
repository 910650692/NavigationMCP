/*
 * Copyright © 2020 SAIC MOTOR Z-ONE SOFTWARE COMPANY. All rights reserved.
 */

package com.fy.navi.hmi.mapdata.adapter;

import android.view.View;
import android.view.ViewGroup;

import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityItemBean;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.List;

public abstract class BaseSearchMapDataAdapter<G, S extends BaseRecyclerHolder>
        extends RecyclerView.Adapter<BaseRecyclerHolder> {

    private List<Boolean> groupItemStatus = new ArrayList<>();// 保存 groupItem 状态，开还是关
    private List<DataTree> dataTrees = new ArrayList<>();

    public List getData() {
        return dataTrees;
    }

    /**
     * Get new SubItem data for adapter.
     * 适配器显示新的数据。它必须被设置时，新的数据。
     * @param position New data
     */
    public List<CityItemBean> getSubItem(int position) {
        if (position >= 0 && position < dataTrees.size()) {
            return dataTrees.get(position).getSubItems();
        } else {
            return null;
        }
    }

    /**
     * Set new data for adapter to show. It must be called when set new data.
     * 适配器显示新的数据。它必须被设置时，新的数据。
     * @param data New data
     */
    public void notifyNewData(List data) {
        setDataTrees(data);
    }

    /**
     * Set new data for adapter and notify changing.
     * 建立新的数据适配器和通知的变化。
     * @param dt New data
     */
    private final void setDataTrees(List dt) {
        this.dataTrees = dt;
        initGroupItemStatus(groupItemStatus);
        notifyDataSetChanged();
    }

    /**
     * Initialize the list to false.
     * 设置初始值，所有 groupItem 默认为关闭状态。
     * @param list The list need to initialize
     */
    private void initGroupItemStatus(List list) {
        for (int i = 0; i < dataTrees.size(); i++) {
            //list.add(false);
            //当匹配关键字时，只展示当前城市列表或省份列表
            if (dataTrees.size() == 1) {
                if (dataTrees.get(i).getGroupItem().equals("")) {
                    boolean type = Boolean.parseBoolean((list.get(i)).toString());
                    if (!type) {
                        list.add(i, true);
                    }
                } else {
                    list.add(i, false);
                }
            } else {
                list.add(i,false);
            }
        }
    }

    /**
     * Create group item view holder for onCreateViewHolder.
     *
     * @param parent Provided by onCreateViewHolder.
     */
    public abstract BaseRecyclerHolder groupItemViewHolder(ViewGroup parent);

    /**
     * Create subitem view holder for onCreateViewHolder.
     *
     * @param parent Provided by onCreateViewHolder.
     */
    public abstract BaseRecyclerHolder subItemViewHolder(ViewGroup parent);

    @Override
    public final BaseRecyclerHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        BaseRecyclerHolder viewHolder = null;
        //根据不同的由 getItemViewType() 返回的 viewType 选择不同的项目布局
        if (viewType == ItemStatus.VIEW_TYPE_GROUP_ITEM) {
            //显示一级列表
            viewHolder = groupItemViewHolder(parent);
        } else if (viewType == ItemStatus.VIEW_TYPE_SUB_ITEM) {
            //显示二级列表
            viewHolder = subItemViewHolder(parent);
        }
        return viewHolder;
    }

    /**
     * Update the content of specified group item. The method will called by onBindViewHolder.
     *
     * @param holder         The ViewHolder which should be updated to represent the contents of the
     *                       item at the given position in the data set.
     *                       一级视图绑定
     * @param groupItemIndex The index of the group item.
     */
    public abstract void onGroupItemBindViewHolder(BaseRecyclerHolder holder, int
            groupItemIndex);

    /**
     * Update the content of specified subitem. The method will called by onBindViewHolder.
     *
     * @param holder       The ViewHolder which should be updated to represent the contents of the
     *                     item at the given position in the data set.
     * @param subItemIndex The index of the subitem.
     *                     二级列表视图绑定
     */
    public abstract void onSubItemBindViewHolder(BaseRecyclerHolder holder, int
            groupItemIndex, int subItemIndex);

    /**
     * The method will be called when the group item clicked.
     *
     * @param isExpand       whether is expanded or no the group item clicked.
     * @param holder         The holder' s item view clicked.
     * @param groupItemIndex The index of the group item clicked.
     *                       一级条目点击事件
     */
    public abstract void onGroupItemClick(Boolean isExpand, G holder, int groupItemIndex);

    /**
     * The method will be called when the subitem clicked.
     *
     * @param holder       The holder' s item view clicked.
     * @param subItemIndex The index of the subitem clicked.
     *                     二级条目点击事件
     **/
    public abstract void onSubItemClick(S holder, int groupItemIndex, int subItemIndex);

    @Override
    public final void onBindViewHolder(final BaseRecyclerHolder holder, int position) {
        final ItemStatus itemStatus = getItemStatusByPosition(position);
        final DataTree dt = dataTrees.get(itemStatus.getGroupItemIndex());
        if (itemStatus.getViewType() == ItemStatus.VIEW_TYPE_GROUP_ITEM) {
            onGroupItemBindViewHolder(holder, itemStatus.getGroupItemIndex());
            holder.itemView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    int areaType = ((ProvDataInfo) dt.getGroupItem()).areaType;
                    if (areaType == 1) {
                        int groupItemIndex = itemStatus.getGroupItemIndex();
                        if (!groupItemStatus.get(groupItemIndex)) {
                            onGroupItemClick(false, (G) holder, groupItemIndex);
                            groupItemStatus.set(groupItemIndex, true);//groupItem由“关闭”状态到“打开”状态
                            notifyItemRangeInserted(holder.getAdapterPosition() + 1, dt.getSubItems().size());
                        } else {
                            onGroupItemClick(true, (G) holder, groupItemIndex);
                            groupItemStatus.set(groupItemIndex, false);//groupItem由“打开”状态到“关闭”状态
                            notifyItemRangeRemoved(holder.getAdapterPosition() + 1, dt.getSubItems().size());
                        }
                    }

                }
            });
        } else if (itemStatus.getViewType() == ItemStatus.VIEW_TYPE_SUB_ITEM) {
            onSubItemBindViewHolder(holder, itemStatus.getGroupItemIndex(), itemStatus.getSubItemIndex());
           /* holder.getView(R.id.offline_status).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onSubItemClick((S) holder, itemStatus.getGroupItemIndex(), itemStatus.getSubItemIndex());
                }
            });*/
        }
    }

    @Override
    public final int getItemCount() {
        int itemCount = 0;
        if (groupItemStatus.size() == 0) {
            return 0;
        }
        for (int i = 0; i < dataTrees.size(); i++) {
            if (groupItemStatus.get(i)) {
                itemCount += dataTrees.get(i).getSubItems().size() + 1;
            } else {
                itemCount++;
            }
        }
        return itemCount;
    }

    @Override
    public final int getItemViewType(int position) {
        return getItemStatusByPosition(position).getViewType();
    }

    /**
     * Get item' s status include view type, group item index and sub_item index.
     * 根据 position 来计算判断该 item 的状态，返回一个 ItemStatus
     * @param position Position
     */
    public ItemStatus getItemStatusByPosition(int position) {

        ItemStatus itemStatus = new ItemStatus();

        int count = 0;//计算groupItemIndex = i 时，position最大值
        int index = 0;

        //轮询 groupItem 的开关状态
        for (index = 0; index < groupItemStatus.size(); index++) {

            //pos刚好等于计数时，item为groupItem
            if (count == position) {
                itemStatus.setViewType(ItemStatus.VIEW_TYPE_GROUP_ITEM);
                itemStatus.setGroupItemIndex(index);
                break;

            //pos大于计数时，item为groupItem(i - 1)中的某个subItem
            } else if (count > position) {
                itemStatus.setViewType(ItemStatus.VIEW_TYPE_SUB_ITEM);
                itemStatus.setGroupItemIndex(index - 1);
                itemStatus.setSubItemIndex(position - (count - dataTrees.get(index - 1).getSubItems().size()));
                break;
            }

            count++;//无论groupItem状态是开或者关，它在列表中都会存在，所有count++

            //当轮询到的groupItem的状态为“开”的话，count需要加上该groupItem下面的子项目数目
            if (groupItemStatus.get(index)) {
                count += dataTrees.get(index).getSubItems().size();
            }
        }

        //简单地处理当轮询到最后一项groupItem的时候
        if (index >= groupItemStatus.size()) {
            itemStatus.setGroupItemIndex(index - 1);
            itemStatus.setViewType(ItemStatus.VIEW_TYPE_SUB_ITEM);
            itemStatus.setSubItemIndex(position - (count - dataTrees.get(index - 1).getSubItems().size()));
        }

        return itemStatus;
    }

    /**
     * ItemStatus 用封装列表每一项的状态，包括：viewType,groupItemIndex,subItemIndex.
     */
    public static class ItemStatus {

        public static final int VIEW_TYPE_GROUP_ITEM = 0;
        public static final int VIEW_TYPE_SUB_ITEM = 1;

        private int viewType; // item的类型，group item 还是 sub item
        private int groupItemIndex = 0; // 一级索引位置
        private int subItemIndex = -1; // 如果该 item 是一个二级子项目，则保存子项目索引

        public ItemStatus() {
        }

        public int getViewType() {
            return viewType;
        }

        public void setViewType(int viewType) {
            this.viewType = viewType;
        }

        public int getGroupItemIndex() {
            return groupItemIndex;
        }

        public void setGroupItemIndex(int groupItemIndex) {
            this.groupItemIndex = groupItemIndex;
        }

        public int getSubItemIndex() {
            return subItemIndex;
        }

        public void setSubItemIndex(int subItemIndex) {
            this.subItemIndex = subItemIndex;
        }
    }


    public static final class DataTree<K, V> {

        private K groupItem;
        private List<CityDataInfo> subItems;

        public DataTree(K groupItem, List<CityDataInfo> subItems) {
            this.groupItem = groupItem;
            this.subItems = subItems;
        }

        public K getGroupItem() {
            return groupItem;
        }

        public List<CityDataInfo> getSubItems() {
            return subItems;
        }
    }
}

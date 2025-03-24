package com.fy.navi.hmi.mapdata.adapter;

import android.view.View;
import android.view.ViewGroup;
import androidx.recyclerview.widget.RecyclerView;
import com.fy.navi.hmi.R;
import com.fy.navi.service.define.mapdata.CityDataInfo;

import java.util.ArrayList;
import java.util.List;

public abstract class MuliteRecycleAdapter<G, S extends BaseRecyclerHolder>
        extends RecyclerView.Adapter<BaseRecyclerHolder> {

    private List<Boolean> mGroupItemStatus = new ArrayList<>();// 保存 groupItem 状态，开还是关
    private List<DataTree> mDataTrees = new ArrayList<>();

    public List getData() {
        return mDataTrees;
    }

    /**
     *  * Get new SubItem data for adapter.
     *  * 适配器显示新的数据。它必须被设置时，新的数据。
     * @param position
     * @return 返回二级列表信息
     */
    public List<CityDataInfo> getSubItem(final int position) {
        if (position >= 0 && position < mDataTrees.size()) {
            return mDataTrees.get(position).getSubItems();
        } else {
            return null;
        }
    }

    /**
     * Set new data for adapter to show. It must be called when set new data.
     * 适配器显示新的数据。它必须被设置时，新的数据。
     * @param data New data
     */
    public void notifyNewData(final List data) {
        setDataTrees(data);
    }

    /**
     * Set new data for adapter and notify changing.
     * 建立新的数据适配器和通知的变化。
     * @param dt New data
     */
    private void setDataTrees(final List dt) {
        this.mDataTrees = dt;
        initGroupItemStatus(mGroupItemStatus);
        notifyDataSetChanged();
    }

    /**
     * Initialize the list to false.
     * 设置初始值，所有 groupItem 默认为关闭状态。
     * @param list The list need to initialize
     */
    private void initGroupItemStatus(final List list) {
        for (int i = 0; i < mDataTrees.size(); i++) {
            //list.add(false);
            //当匹配关键字时，只展示当前城市列表或省份列表
            if (mDataTrees.size() == 1) {
                if (mDataTrees.get(i).getGroupItem().equals("")) {
                    final boolean type = Boolean.parseBoolean((list.get(i)).toString());
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
     *  Create group item view holder for onCreateViewHolder.
     * @param parent
     * @return 返回一级item view
     */
    public abstract BaseRecyclerHolder groupItemViewHolder(final ViewGroup parent);

    /**
     * * Create subitem view holder for onCreateViewHolder.
     * @param parent
     * @return 返回二级item view
     */
    public abstract BaseRecyclerHolder subItemViewHolder(final ViewGroup parent);

    @Override
    public final BaseRecyclerHolder onCreateViewHolder(final ViewGroup parent, final int viewType) {
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
     * 一级视图绑定
     *
     * @param holder
     * @param groupItemIndex
     */
    public abstract void onGroupItemBindViewHolder(final BaseRecyclerHolder holder,final int groupItemIndex);

    /**
     * 二级列表视图绑定
     *
     * @param holder
     * @param groupItemIndex
     * @param subItemIndex
     */
    public abstract void onSubItemBindViewHolder(final BaseRecyclerHolder holder, final int
            groupItemIndex, final int subItemIndex);

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
     *  二级条目点击事件
     *
     * @param holder       The holder' s item view clicked.
     * @param groupItemIndex       The holder' s item view clicked.
     * @param subItemIndex The index of the subitem clicked.
     **/
    public abstract void onSubItemClick(final S holder, final int groupItemIndex, final int subItemIndex);

    /**
     *  二级条目点击删除事件
     *
     * @param holder       The holder' s item view clicked.
     * @param groupItemIndex       The holder' s item view clicked.
     * @param subItemIndex The index of the subitem clicked.
     **/
    public abstract void onSubItemDeleteClick(final S holder, final int groupItemIndex, final int subItemIndex);

    @Override
    public final void onBindViewHolder(final BaseRecyclerHolder holder, final int position) {
        final ItemStatus itemStatus = getItemStatusByPosition(position);
        final DataTree dt = mDataTrees.get(itemStatus.getGroupItemIndex());
        if (itemStatus.getViewType() == ItemStatus.VIEW_TYPE_GROUP_ITEM) {
            onGroupItemBindViewHolder(holder, itemStatus.getGroupItemIndex());
            holder.itemView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(final View v) {
                    final int groupItemIndex = itemStatus.getGroupItemIndex();
                    if (!mGroupItemStatus.get(groupItemIndex)) {
                        onGroupItemClick(false, (G) holder, groupItemIndex);
                        mGroupItemStatus.set(groupItemIndex, true);//groupItem由“关闭”状态到“打开”状态
                        notifyItemRangeInserted(holder.getAdapterPosition() + 1, dt.getSubItems().size());
                    } else {
                        onGroupItemClick(true, (G) holder, groupItemIndex);
                        mGroupItemStatus.set(groupItemIndex, false);//groupItem由“打开”状态到“关闭”状态
                        notifyItemRangeRemoved(holder.getAdapterPosition() + 1, dt.getSubItems().size());
                    }
                }
            });
        } else if (itemStatus.getViewType() == ItemStatus.VIEW_TYPE_SUB_ITEM) {
            onSubItemBindViewHolder(holder, itemStatus.getGroupItemIndex(), itemStatus.getSubItemIndex());
            holder.getView(R.id.item_download_status).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(final View v) {
                    onSubItemClick((S) holder, itemStatus.getGroupItemIndex(), itemStatus.getSubItemIndex());
                }
            });

            holder.getView(R.id.item_driving_delete).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(final View v) {
                    onSubItemDeleteClick((S) holder, itemStatus.getGroupItemIndex(), itemStatus.getSubItemIndex());
                }
            });
        }
    }

    @Override
    public final int getItemCount() {
        int itemCount = 0;
        if (mGroupItemStatus.size() == 0) {
            return 0;
        }
        for (int i = 0; i < mDataTrees.size(); i++) {
            if (mGroupItemStatus.get(i)) {
                itemCount += mDataTrees.get(i).getSubItems().size() + 1;
            } else {
                itemCount++;
            }
        }
        return itemCount;
    }

    @Override
    public final int getItemViewType(final int position) {
        return getItemStatusByPosition(position).getViewType();
    }

    /**
     * * Get item' s status include view type, group item index and sub_item index.
     *  * 根据 position 来计算判断该 item 的状态，返回一个 ItemStatus
     * @param position
     * @return 返回item 状态
     */
    public ItemStatus getItemStatusByPosition(final int position) {

        final ItemStatus itemStatus = new ItemStatus();

        int count = 0;//计算groupItemIndex = i 时，position最大值
        int index = 0;

        //轮询 groupItem 的开关状态
        for (index = 0; index < mGroupItemStatus.size(); index++) {

            //pos刚好等于计数时，item为groupItem
            if (count == position) {
                itemStatus.setViewType(ItemStatus.VIEW_TYPE_GROUP_ITEM);
                itemStatus.setGroupItemIndex(index);
                break;

            //pos大于计数时，item为groupItem(i - 1)中的某个subItem
            } else if (count > position) {
                itemStatus.setViewType(ItemStatus.VIEW_TYPE_SUB_ITEM);
                itemStatus.setGroupItemIndex(index - 1);
                itemStatus.setSubItemIndex(position - (count - mDataTrees.get(index - 1).getSubItems().size()));
                break;
            }

            count++;//无论groupItem状态是开或者关，它在列表中都会存在，所有count++

            //当轮询到的groupItem的状态为“开”的话，count需要加上该groupItem下面的子项目数目
            if (mGroupItemStatus.get(index)) {
                count += mDataTrees.get(index).getSubItems().size();
            }
        }

        //简单地处理当轮询到最后一项groupItem的时候
        if (index >= mGroupItemStatus.size()) {
            itemStatus.setGroupItemIndex(index - 1);
            itemStatus.setViewType(ItemStatus.VIEW_TYPE_SUB_ITEM);
            itemStatus.setSubItemIndex(position - (count - mDataTrees.get(index - 1).getSubItems().size()));
        }

        return itemStatus;
    }

    /**
     * ItemStatus 用封装列表每一项的状态，包括：viewType,groupItemIndex,subItemIndex.
     */
    public static class ItemStatus {

        public static final int VIEW_TYPE_GROUP_ITEM = 0;
        public static final int VIEW_TYPE_SUB_ITEM = 1;

        private int mViewType; // item的类型，group item 还是 sub item
        private int mGroupItemIndex = 0; // 一级索引位置
        private int mSubItemIndex = -1; // 如果该 item 是一个二级子项目，则保存子项目索引

        public ItemStatus() {
        }

        public int getViewType() {
            return mViewType;
        }

        public void setViewType(final int viewType) {
            this.mViewType = viewType;
        }

        public int getGroupItemIndex() {
            return mGroupItemIndex;
        }

        public void setGroupItemIndex(final int groupItemIndex) {
            this.mGroupItemIndex = groupItemIndex;
        }

        public int getSubItemIndex() {
            return mSubItemIndex;
        }

        public void setSubItemIndex(final int subItemIndex) {
            this.mSubItemIndex = subItemIndex;
        }
    }


    public static final class DataTree<K, V> {

        private K mGroupItem;
        private List<CityDataInfo> mSubItems;

        public DataTree(final K groupItem, final List<CityDataInfo> subItems) {
            this.mGroupItem = groupItem;
            this.mSubItems = subItems;
        }

        public K getGroupItem() {
            return mGroupItem;
        }

        public List<CityDataInfo> getSubItems() {
            return mSubItems;
        }
    }
}

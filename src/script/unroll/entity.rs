/// Kind of an entity. Can be all primitives + a bind.
#[derive(Debug)]
pub enum EntityKind {
    /// A single real value (possibly delimited).
    Scalar,
    Line,
    Point,
    /// Set of points within the same distance of the circle's origin.
    Circle,
    /// Non-delimited set
    All
}

/// An entity is a single primitive on the figure plane.
#[derive(Debug)]
pub struct Entity {
    /// The kind of this entity.
    pub kind: EntityKind,
    /// The index of its parent.
    pub parent: EntityHandle
}

impl Entity {
    pub fn free_point() -> Self {
        Self {
            kind: EntityKind::Point,
            parent: EntityHandle::Indexed(0)
        }
    }

    pub fn free_scalar() -> Self {
        Self {
            kind: EntityKind::Point,
            parent: EntityHandle::Indexed(0)
        }
    }
}

#[derive(Debug)]
pub enum EntityHandle {
    Indexed(usize),
    Bind(UnrolledExpression)
}

#[derive(Debug)]
pub struct CircleHandle<'r> {
    handle: EntityHandle,
    context: &'r mut CompileContext
}

impl<'r> CircleHandle<'r> {
    pub fn equals(&mut self, rhs: UnrolledExpression) {
        match &self.handle {
            EntityHandle::Indexed(i) => match &mut self.context.entities[self.context.entities[*i].parent].kind {
                EntityKind::Bind(_) => todo!(),
                v @ EntityKind::All => *v = EntityKind::Bind(rhs),
                _ => unreachable!()
            },
            EntityHandle::Bind(_) => todo!(),
        }
        
    }
}

#[derive(Debug)]
pub struct PointHandle<'r> {
    index: EntityHandle,
    context: &'r mut CompileContext
}

impl<'r> PointHandle<'r> {
    pub fn equals(&mut self, rhs: UnrolledExpression) {
        match &mut self.context.entities[self.context.entities[self.index].parent].kind {
            EntityKind::Bind(_) => todo!(),
            v @ EntityKind::All => *v = EntityKind::Bind(rhs),
            _ => unreachable!()
        }
    }

    pub fn lies_on(&mut self, rhs: UnrolledExpression) {
        match &mut self.context.entities[self.context.entities[self.index].parent].kind {
            v @ EntityKind::All => *v = EntityKind::Circle,
            _ => todo!("If not a free, turn into a rule.")
        }
    }
}